//! Module for managing Document Errors as [`DocError`]
use crate::{comments::CommentError, docs::TableDoc};
use core::fmt;
use sqlparser::parser::ParserError;
use std::{error, fmt::Debug};
/// Error enum for returning relevant error based on error type
#[derive(Debug)]
pub enum DocError {
    /// Wrapper for standard [`std::io::Error`]
    FileReadError(std::io::Error),
    /// Wrapper for [`CommentError`]
    CommentError(CommentError),
    /// Wrapper for [`ParserError`]
    SqlParserError(ParserError),
    /// Indicates an invalid or unexpected object name in the [`sqlparser::ast::Statement`]
    InvalidObjectName {
        /// The message to accompany the invalid [`sqlparser::ast::ObjectName`]
        message: String,
        /// The line number for the invalid [`sqlparser::ast::ObjectName`]
        line: u64,
        /// The column number for the invalid [`sqlparser::ast::ObjectName`]
        column: u64,
    },
    /// Table not found when searching [`crate::SqlDoc`]
    TableNotFound {
        /// The name of the table that was not found
        name: String,
    },
    /// Duplicate tables with same name were found when searching [`crate::SqlDoc`]
    DuplicateTablesFound {
        /// `Vec` of the [`TableDoc`] for each duplicate table found
        tables: Vec<TableDoc>,
    },
}

impl fmt::Display for DocError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FileReadError(error) => write!(f, "file read error: {error}"),
            Self::CommentError(comment_error) => {
                write!(f, "comment parse error: {comment_error}")
            }
            Self::SqlParserError(parser_error) => write!(f, "SQL parse error {parser_error}"),
            Self::InvalidObjectName { message, line, column } => {
                write!(f, "{message} at line {line}, column {column}")
            }
            Self::TableNotFound { name } => write!(f, "Table not found in SqlDoc: {name}"),
            Self::DuplicateTablesFound { tables } => {
                writeln!(f, "Duplicate tables found:")?;
                for t in tables {
                    writeln!(f, "{t}")?;
                }
                Ok(())
            }
        }
    }
}

impl error::Error for DocError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Self::FileReadError(e) => Some(e),
            Self::CommentError(e) => Some(e),
            Self::SqlParserError(e) => Some(e),
            Self::InvalidObjectName { .. }
            | Self::TableNotFound { .. }
            | Self::DuplicateTablesFound { .. } => None,
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

#[cfg(test)]
mod tests {
    use sqlparser::parser::ParserError;

    use crate::{
        error::DocError, comments::CommentError
    };

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
