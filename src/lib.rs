//! Module layout:
//! - [`files`]    : discover and load `.sql` files from disk
//! - [`ast`]      : parse SQL into an AST using [`sqlparser`]
//! - [`comments`] : extract and model comments and spans

use core::fmt;
use std::{
    error,
    path::{Path, PathBuf},
};

use sqlparser::parser::ParserError;

use crate::{
    ast::ParsedSqlFileSet,
    comments::{CommentError, Comments},
    docs::SqlDocs,
    files::SqlFileSet,
};
pub mod ast;
pub mod comments;
pub mod docs;
pub mod files;

/// Error enum for returning relevant error based on error type
#[derive(Debug)]
pub enum DocError {
    /// Wrapper for standard [`std::io::Error`]
    FileReadError(std::io::Error),
    /// Wrapper for [`CommentError`]
    CommentError(CommentError),
    /// Wrapper for [`ParserError`]
    SqlParserError(ParserError),
}

impl fmt::Display for DocError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FileReadError(error) => write!(f, "file read error:{error}"),
            Self::CommentError(comment_error) => {
                write!(f, "comment parse error: {comment_error}")
            }
            Self::SqlParserError(parser_error) => write!(f, "SQL parse error {parser_error}"),
        }
    }
}

impl error::Error for DocError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Self::FileReadError(e) => Some(e),
            Self::CommentError(e) => Some(e),
            Self::SqlParserError(e) => Some(e),
        }
    }
}

impl From<std::io::Error> for DocError {
    fn from(e: std::io::Error) -> Self {
        Self::FileReadError(e)
    }
}

impl From<CommentError> for DocError {
    fn from(e: CommentError) -> Self {
        Self::CommentError(e)
    }
}

impl From<ParserError> for DocError {
    fn from(e: ParserError) -> Self {
        Self::SqlParserError(e)
    }
}

/// Primary Entry point. Returns a tuple of [`PathBuf`] and [`SqlDocs`].
///
/// # Parameters:
/// - `dir`: the [`Path`] to recursively parse `.sql` files. Allows for coercion
///   from [`String`]
/// - `deny_list`: a `Vec` of the `.sql` files to ignore (do not need to specify
///   other file types to ignore).
///
/// # Errors
/// - Will return a `DocError` that  specifies the error (io, comment parsing,
///   sql parsing)
pub fn generate_docs_from_dir<P: AsRef<Path>, S: AsRef<str>>(
    dir: P,
    deny_list: &[S],
) -> Result<Vec<(PathBuf, SqlDocs)>, DocError> {
    // Convert deny list to a `Vec<String>`
    let deny_vec: Vec<String> = deny_list.iter().map(|file| file.as_ref().to_string()).collect();
    // verify whether deny_list is empty and return correct `Option`
    let deny_option = if deny_vec.is_empty() { None } else { Some(deny_vec) };
    // Generate the file set from the directory content
    let file_set = SqlFileSet::new(dir.as_ref(), deny_option)?;
    // parse all files sql
    let parsed_files = ParsedSqlFileSet::parse_all(file_set)?;
    let mut sql_docs = Vec::new();
    // iterate on each file and generate the `SqlDocs` and associate with the `Path`
    for file in parsed_files.files() {
        let comments = Comments::parse_all_comments_from_file(file)?;
        let docs = SqlDocs::from_parsed_file(file, &comments);
        let path = file.file().path().to_path_buf();
        sql_docs.push((path, docs));
    }
    Ok(sql_docs)
}

/// Secondary Entry point. Returns a tuple of [`PathBuf`] and [`SqlDocs`].
/// Useful when no deny list is needed
///
/// # Parameters:
/// - `dir`: the [`Path`] to recursively parse `.sql` files. Allows for coercion
///   from [`String`]
///
/// # Errors
/// - Will return a `DocError` that  specifies the error (io, comment parsing,
///   sql parsing)
pub fn generate_docs_from_dir_no_deny<P: AsRef<Path>>(
    dir: P,
) -> Result<Vec<(PathBuf, SqlDocs)>, DocError> {
    generate_docs_from_dir::<P, &str>(dir, &[])
}

#[cfg(test)]
#[test]
fn test_with_deny_list_from_files() {
    let generated_docs = generate_docs_from_dir(
        "sql_files",
        &[
            "sql_files/without_comments.sql",
            "sql_files/with_single_line_comments.sql",
            "sql_files/with_multiline_comments.sql",
        ],
    )
    .unwrap();
    assert_eq!(
        generated_docs.iter().next().unwrap().0,
        PathBuf::from("sql_files/with_mixed_comments.sql")
    );
    let table_names = vec!["users", "posts"];
    let table_comments =
        vec!["Users table stores user account information", "Posts table stores blog posts"];
    for (i, (_, sqldoc)) in generated_docs.iter().enumerate() {
        assert_eq!(sqldoc.tables()[i].name(), table_names[i]);
        assert_eq!(sqldoc.tables()[i].doc().as_ref().unwrap(), table_comments[i]);
    }
    let user_columns = vec!["id", "username", "email", "created_at"];
    let user_columns_comments =
        vec!["Primary key", "Username for login", "Email address", "When the user registered"];
    for (i, column) in generated_docs[0].1.tables()[0].columns().iter().enumerate() {
        assert_eq!(column.name(), user_columns[i]);
        assert_eq!(column.doc().as_ref().unwrap(), user_columns_comments[i]);
    }
}

#[test]
fn test_with_no_deny_list_from_files() {
    let generated_docs = generate_docs_from_dir_no_deny("sql_files").unwrap();
    let expected_paths = vec![
        "sql_files/without_comments.sql",
        "sql_files/with_multiline_comments.sql",
        "sql_files/with_single_line_comments.sql",
        "sql_files/with_mixed_comments.sql",
    ];
    let table_names = vec!["users", "posts"];
    let table_comments =
        vec!["Users table stores user account information", "Posts table stores blog posts"];
    let user_columns = vec!["id", "username", "email", "created_at"];
    let user_columns_comments =
        vec!["Primary key", "Username for login", "Email address", "When the user registered"];
    for (i, (buf, sql_docs)) in generated_docs.iter().enumerate() {
        assert_eq!(buf, expected_paths[i]);
        if buf == "sql_files/with_mixed_comments.sql" {
            for (i, table) in sql_docs.tables().iter().enumerate() {
                assert_eq!(table.name(), table_names[i]);
                assert_eq!(table.doc().as_ref().unwrap(), table_comments[i]);
                if table.name() == "users" {
                    for (i, column) in table.columns().iter().enumerate() {
                        assert_eq!(column.name(), user_columns[i]);
                        assert_eq!(column.doc().as_ref().unwrap(), user_columns_comments[i]);
                    }
                }
            }
        }
    }
}
