#![doc = include_str!("../README.md")]
//! Module layout:
//! - [`files`]    : discover and load `.sql` files from disk
//! - [`ast`]      : parse SQL into an AST using [`sqlparser`]
//! - [`comments`] : extract and model comments and spans

use core::fmt;
use std::{
    error,
    fmt::Debug,
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
            Self::FileReadError(error) => write!(f, "file read error: {error}"),
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
///
/// # Example
/// ```rust, no_run
/// use sql_docs::generate_docs_from_dir;
/// let docs = generate_docs_from_dir("sql_dir", &["skip_this.sql"]);
/// ```
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
///
/// # Example
/// ```rust,no_run
/// use sql_docs::generate_docs_from_dir_no_deny;
/// let docs = generate_docs_from_dir_no_deny("sql_dir");
/// ```
pub fn generate_docs_from_dir_no_deny<P: AsRef<Path>>(
    dir: P,
) -> Result<Vec<(PathBuf, SqlDocs)>, DocError> {
    generate_docs_from_dir::<P, &str>(dir, &[])
}

#[cfg(test)]
mod tests {
    use sqlparser::parser::ParserError;

    use crate::{
        DocError, comments::CommentError, generate_docs_from_dir, generate_docs_from_dir_no_deny,
    };

    #[test]
    fn test_with_deny_list_from_files() -> Result<(), Box<dyn std::error::Error>> {
        let generated_docs = generate_docs_from_dir(
            "sql_files",
            &[
                "sql_files/without_comments.sql",
                "sql_files/with_single_line_comments.sql",
                "sql_files/with_multiline_comments.sql",
            ],
        )?;
        let first_path = generated_docs
            .first()
            .map_or_else(|| panic!("unable to find first value"), |(path, _)| path);
        let first_name = first_path
            .file_name()
            .and_then(|s| s.to_str())
            .map_or_else(|| panic!("unable to stringify path"), |val| val);
        assert_eq!(first_name, "with_mixed_comments.sql");
        let table_names = ["users", "posts"];
        let table_comments =
            ["Users table stores user account information", "Posts table stores blog posts"];
        for (i, (_, sqldoc)) in generated_docs.iter().enumerate() {
            assert_eq!(sqldoc.tables()[i].name(), table_names[i]);
            let sqldoc_table_doc = sqldoc.tables()[i]
                .doc()
                .as_ref()
                .map_or_else(|| panic!("unable to find sqldoc table doc"), |val| val);
            assert_eq!(sqldoc_table_doc, table_comments[i]);
        }
        let user_columns = ["id", "username", "email", "created_at"];
        let user_columns_comments =
            ["Primary key", "Username for login", "Email address", "When the user registered"];
        let first_tables = generated_docs
            .first()
            .map_or_else(|| panic!("unable to find first value"), |(_, docs)| docs.tables());
        for (i, column) in first_tables[0].columns().iter().enumerate() {
            assert_eq!(column.name(), user_columns[i]);
            let column_doc = column
                .doc()
                .as_ref()
                .map_or_else(|| panic!("unable to find column doc value"), |val| val);
            assert_eq!(column_doc, user_columns_comments[i]);
        }
        Ok(())
    }

    #[test]
    fn test_with_no_deny_list_from_files() {
        use std::path::Path;
        let parent_path = Path::new("sql_files");
        let Ok(generated_docs) = generate_docs_from_dir_no_deny("sql_files") else {
            panic!("unable to locate test dir");
        };
        let mut actual_paths: Vec<String> =
            generated_docs.iter().map(|(path, _)| path.to_string_lossy().into_owned()).collect();
        actual_paths.sort();
        let mut expected_paths: Vec<String> = vec![
            "without_comments.sql".to_string(),
            "with_multiline_comments.sql".to_string(),
            "with_single_line_comments.sql".to_string(),
            "with_mixed_comments.sql".to_string(),
        ]
        .into_iter()
        .map(|f| parent_path.join(f).to_string_lossy().into_owned())
        .collect();
        expected_paths.sort();
        assert_eq!(actual_paths, expected_paths);
        let target = parent_path.join("with_mixed_comments.sql");
        let Some((_, mixed_docs)) =
            generated_docs.iter().find(|(path, _)| path.as_path() == target)
        else {
            panic!("with_mixed_comments.sql should be present");
        };
        let table_names = ["users", "posts"];
        let table_comments =
            ["Users table stores user account information", "Posts table stores blog posts"];
        let user_columns = ["id", "username", "email", "created_at"];
        let user_columns_comments =
            ["Primary key", "Username for login", "Email address", "When the user registered"];
        for (i, table) in mixed_docs.tables().iter().enumerate() {
            assert_eq!(table.name(), table_names[i]);

            match table.doc().as_ref() {
                Some(val) => assert_eq!(val, &table_comments[i]),
                None => panic!("There should be a value for the table doc"),
            }

            if table.name() == "users" {
                for (i, column) in table.columns().iter().enumerate() {
                    assert_eq!(column.name(), user_columns[i]);
                    match column.doc().as_ref() {
                        Some(val) => assert_eq!(val, &user_columns_comments[i]),
                        None => panic!("there should be a value for the doc column"),
                    }
                }
            }
        }
    }

    #[test]
    fn test_doc_errors() {
        use std::fs;

        use crate::comments::Location;
        let Err(io_error) = fs::read_dir("INVALID") else {
            panic!("there should not be a directory called INVALID")
        };
        let io_error_str = io_error.to_string();
        let read_error = DocError::FileReadError(io_error);
        let expected_read_error = "file read error: ".to_owned() + &io_error_str;
        assert!(read_error.to_string().contains(&expected_read_error));

        let comment_error = DocError::CommentError(CommentError::UnmatchedMultilineCommentStart {
            location: Location::default(),
        });
        let expected_comment_error =
            "comment parse error: unmatched block comment start at line 1, column 1";
        assert_eq!(comment_error.to_string(), expected_comment_error);

        let sql_error = DocError::SqlParserError(ParserError::RecursionLimitExceeded);
        let expected_sql_error = "SQL parse error sql parser error: recursion limit exceeded";
        assert_eq!(sql_error.to_string(), expected_sql_error);
    }

    #[test]
    fn test_doc_errors_from() {
        use std::fs;

        use crate::comments::Location;
        let Err(io_error) = fs::read_dir("INVALID") else {
            panic!("there should not be a directory called INVALID")
        };
        let io_kind = io_error.kind();
        let doc_io_error = DocError::from(io_error);
        match doc_io_error {
            DocError::FileReadError(inner) => assert_eq!(inner.kind(), io_kind),
            _ => panic!("expected instance of DocError::FileReadError"),
        }

        let comment_error =
            CommentError::UnmatchedMultilineCommentStart { location: Location::default() };
        let comment_error_str = comment_error.to_string();
        let doc_comment_error: DocError = comment_error.into();
        match doc_comment_error {
            DocError::CommentError(inner) => assert_eq!(inner.to_string(), comment_error_str),
            _ => panic!("expected instance of DocError::CommentError"),
        }

        let parser_error = ParserError::RecursionLimitExceeded;
        let parser_error_str = parser_error.to_string();
        let doc_parser_error: DocError = parser_error.into();
        match doc_parser_error {
            DocError::SqlParserError(inner) => assert_eq!(inner.to_string(), parser_error_str),
            _ => panic!("expected instance of DocError::SqlParserError"),
        }
    }

    #[test]
    fn test_doc_error_source() {
        use std::{error::Error, fs};

        use crate::comments::Location;

        let Err(io_err) = fs::read_dir("INVALID") else {
            panic!("there should not be a directory called INVALID")
        };
        let io_err_str = io_err.to_string();
        let doc_io = DocError::FileReadError(io_err);
        let src = doc_io
            .source()
            .map_or_else(|| panic!("expected Some(source) for FileReadError"), |source| source);
        assert_eq!(src.to_string(), io_err_str);

        let comment =
            CommentError::UnmatchedMultilineCommentStart { location: Location::default() };
        let comment_str = comment.to_string();
        let doc_comment = DocError::CommentError(comment);
        let src = doc_comment
            .source()
            .map_or_else(|| panic!("expected Some(source) for CommentError"), |source| source);
        assert_eq!(src.to_string(), comment_str);

        let parser = ParserError::RecursionLimitExceeded;
        let parser_str = parser.to_string();
        let doc_parser = DocError::SqlParserError(parser);
        let src = doc_parser
            .source()
            .map_or_else(|| panic!("expected Some(source) for SqlParserError"), |source| source);
        assert_eq!(src.to_string(), parser_str);
    }
}
