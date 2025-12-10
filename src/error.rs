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
