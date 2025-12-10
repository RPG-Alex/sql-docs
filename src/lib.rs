#![doc = include_str!("../README.md")]
//! Module layout:
//! - [`files`]    : discover and load `.sql` files from disk
//! - [`ast`]      : parse SQL into an AST using [`sqlparser`]
//! - [`comments`] : extract and model comments and spans
//! - [`docs`]     : Generates a struct to hold [`docs::TableDoc`]
//! - [`sql_doc`]  : Creates the [`SqlDoc`] structure

pub mod ast;
pub mod comments;
pub mod docs;
pub mod error;
pub mod files;
mod sql_doc;
pub use sql_doc::SqlDoc;

// #[cfg(test)]
// mod tests {
//     use sqlparser::parser::ParserError;

//     use crate::{
//         error::DocError, comments::CommentError
//     };

//     #[test]
//     fn test_with_deny_list_from_files() -> Result<(), Box<dyn std::error::Error>> {
//         let generated_docs = generate_docs_from_dir(
//             "sql_files",
//             &[
//                 "sql_files/without_comments.sql",
//                 "sql_files/with_single_line_comments.sql",
//                 "sql_files/with_multiline_comments.sql",
//             ],
//         )?;
//         let first_path = generated_docs
//             .first()
//             .map_or_else(|| panic!("unable to find first value"), |(path, _)| path);
//         let first_name = first_path
//             .file_name()
//             .and_then(|s| s.to_str())
//             .map_or_else(|| panic!("unable to stringify path"), |val| val);
//         assert_eq!(first_name, "with_mixed_comments.sql");
//         let table_names = ["users", "posts"];
//         let table_comments =
//             ["Users table stores user account information", "Posts table stores blog posts"];
//         for (i, (_, parsed_doc)) in generated_docs.iter().enumerate() {
//             assert_eq!(parsed_doc.tables()[i].name(), table_names[i]);
//             let parsed_doc_table_doc = parsed_doc.tables()[i]
//                 .doc()
//                 .map_or_else(|| panic!("unable to find SqlFileDoc table doc"), |val| val);
//             assert_eq!(parsed_doc_table_doc, table_comments[i]);
//         }
//         let user_columns = ["id", "username", "email", "created_at"];
//         let user_columns_comments =
//             ["Primary key", "Username for login", "Email address", "When the user registered"];
//         let first_tables = generated_docs
//             .first()
//             .map_or_else(|| panic!("unable to find first value"), |(_, docs)| docs.tables());
//         for (i, column) in first_tables[0].columns().iter().enumerate() {
//             assert_eq!(column.name(), user_columns[i]);
//             let column_doc =
//                 column.doc().map_or_else(|| panic!("unable to find column doc value"), |val| val);
//             assert_eq!(column_doc, user_columns_comments[i]);
//         }
//         Ok(())
//     }

//     #[test]
//     fn test_with_no_deny_list_from_files() {
//         use std::path::Path;
//         let parent_path = Path::new("sql_files");
//         let Ok(generated_docs) = generate_docs_from_dir_no_deny("sql_files") else {
//             panic!("unable to locate test dir");
//         };
//         let mut actual_paths: Vec<String> =
//             generated_docs.iter().map(|(path, _)| path.to_string_lossy().into_owned()).collect();
//         actual_paths.sort();
//         let mut expected_paths: Vec<String> = vec![
//             "without_comments.sql".to_string(),
//             "with_multiline_comments.sql".to_string(),
//             "with_single_line_comments.sql".to_string(),
//             "with_mixed_comments.sql".to_string(),
//         ]
//         .into_iter()
//         .map(|f| parent_path.join(f).to_string_lossy().into_owned())
//         .collect();
//         expected_paths.sort();
//         assert_eq!(actual_paths, expected_paths);
//         let target = parent_path.join("with_mixed_comments.sql");
//         let Some((_, mixed_docs)) =
//             generated_docs.iter().find(|(path, _)| path.as_path() == target)
//         else {
//             panic!("with_mixed_comments.sql should be present");
//         };
//         let table_names = ["users", "posts"];
//         let table_comments =
//             ["Users table stores user account information", "Posts table stores blog posts"];
//         let user_columns = ["id", "username", "email", "created_at"];
//         let user_columns_comments =
//             ["Primary key", "Username for login", "Email address", "When the user registered"];
//         for (i, table) in mixed_docs.tables().iter().enumerate() {
//             assert_eq!(table.name(), table_names[i]);

//             match table.doc().as_ref() {
//                 Some(val) => assert_eq!(val, &table_comments[i]),
//                 None => panic!("There should be a value for the table doc"),
//             }

//             if table.name() == "users" {
//                 for (i, column) in table.columns().iter().enumerate() {
//                     assert_eq!(column.name(), user_columns[i]);
//                     match column.doc().as_ref() {
//                         Some(val) => assert_eq!(val, &user_columns_comments[i]),
//                         None => panic!("there should be a value for the doc column"),
//                     }
//                 }
//             }
//         }
//     }

//     #[test]
//     fn test_doc_errors() {
//         use std::fs;

//         use crate::comments::Location;
//         let Err(io_error) = fs::read_dir("INVALID") else {
//             panic!("there should not be a directory called INVALID")
//         };
//         let io_error_str = io_error.to_string();
//         let read_error = DocError::FileReadError(io_error);
//         let expected_read_error = "file read error: ".to_owned() + &io_error_str;
//         assert!(read_error.to_string().contains(&expected_read_error));

//         let comment_error = DocError::CommentError(CommentError::UnmatchedMultilineCommentStart {
//             location: Location::default(),
//         });
//         let expected_comment_error =
//             "comment parse error: unmatched block comment start at line 1, column 1";
//         assert_eq!(comment_error.to_string(), expected_comment_error);

//         let sql_error = DocError::SqlParserError(ParserError::RecursionLimitExceeded);
//         let expected_sql_error = "SQL parse error sql parser error: recursion limit exceeded";
//         assert_eq!(sql_error.to_string(), expected_sql_error);
//     }

//     #[test]
//     fn test_doc_errors_from() {
//         use std::fs;

//         use crate::comments::Location;
//         let Err(io_error) = fs::read_dir("INVALID") else {
//             panic!("there should not be a directory called INVALID")
//         };
//         let io_kind = io_error.kind();
//         let doc_io_error = DocError::from(io_error);
//         match doc_io_error {
//             DocError::FileReadError(inner) => assert_eq!(inner.kind(), io_kind),
//             _ => panic!("expected instance of DocError::FileReadError"),
//         }

//         let comment_error =
//             CommentError::UnmatchedMultilineCommentStart { location: Location::default() };
//         let comment_error_str = comment_error.to_string();
//         let doc_comment_error: DocError = comment_error.into();
//         match doc_comment_error {
//             DocError::CommentError(inner) => assert_eq!(inner.to_string(), comment_error_str),
//             _ => panic!("expected instance of DocError::CommentError"),
//         }

//         let parser_error = ParserError::RecursionLimitExceeded;
//         let parser_error_str = parser_error.to_string();
//         let doc_parser_error: DocError = parser_error.into();
//         match doc_parser_error {
//             DocError::SqlParserError(inner) => assert_eq!(inner.to_string(), parser_error_str),
//             _ => panic!("expected instance of DocError::SqlParserError"),
//         }
//     }

//     #[test]
//     fn test_doc_error_source() {
//         use std::{error::Error, fs};

//         use crate::comments::Location;

//         let Err(io_err) = fs::read_dir("INVALID") else {
//             panic!("there should not be a directory called INVALID")
//         };
//         let io_err_str = io_err.to_string();
//         let doc_io = DocError::FileReadError(io_err);
//         let src = doc_io
//             .source()
//             .map_or_else(|| panic!("expected Some(source) for FileReadError"), |source| source);
//         assert_eq!(src.to_string(), io_err_str);

//         let comment =
//             CommentError::UnmatchedMultilineCommentStart { location: Location::default() };
//         let comment_str = comment.to_string();
//         let doc_comment = DocError::CommentError(comment);
//         let src = doc_comment
//             .source()
//             .map_or_else(|| panic!("expected Some(source) for CommentError"), |source| source);
//         assert_eq!(src.to_string(), comment_str);

//         let parser = ParserError::RecursionLimitExceeded;
//         let parser_str = parser.to_string();
//         let doc_parser = DocError::SqlParserError(parser);
//         let src = doc_parser
//             .source()
//             .map_or_else(|| panic!("expected Some(source) for SqlParserError"), |source| source);
//         assert_eq!(src.to_string(), parser_str);
//     }
// }
