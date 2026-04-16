//! Error types returned by this crate’s public APIs.

use alloc::{string::String, vec::Vec};
use core::fmt;

use sqlparser::parser::ParserError;

use crate::{
    comments::CommentError,
    docs::{ColumnDoc, TableDoc},
};

/// Errors that can occur while discovering, parsing, or documenting SQL files.
#[non_exhaustive]
#[derive(Debug)]
pub enum DocError {
    /// Wrapper for standard [`std::io::Error`]
    #[cfg(feature = "std")]
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
    /// Column not found when searching [`crate::docs::TableDoc`]
    ColumnNotFound {
        /// The name of the column not found
        name: String,
    },
    /// Duplicate tables with same name were found when searching [`crate::SqlDoc`]
    DuplicateTablesFound {
        /// `Vec` of the [`TableDoc`] for each duplicate table found
        tables: Vec<TableDoc>,
    },
    /// Duplicate columns with same name were found when searching a single [`TableDoc`]
    DuplicateColumnsFound {
        /// `Vec` of the [`crate::docs::ColumnDoc`] for each duplicate table found
        columns: Vec<ColumnDoc>,
    },
    /// Could not find table with `schema`
    TableWithSchemaNotFound {
        /// the name of the table not found
        name: String,
        /// the schema for the table not found
        schema: String,
    },
}

impl core::fmt::Display for DocError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            #[cfg(feature = "std")]
            Self::FileReadError(error) => write!(f, "file read error: {error}"),
            Self::CommentError(comment_error) => {
                write!(f, "comment parse error: {comment_error}")
            }
            Self::SqlParserError(parser_error) => write!(f, "SQL parse error {parser_error}"),
            Self::InvalidObjectName { message, line, column } => {
                write!(f, "{message} at line {line}, column {column}")
            }
            Self::TableNotFound { name } => write!(f, "Table not found in SqlDoc: {name}"),
            Self::ColumnNotFound { name } => write!(f, "Column not found in TableDoc: {name}"),
            Self::DuplicateTablesFound { tables } => {
                writeln!(f, "Duplicate tables found:")?;
                for t in tables {
                    writeln!(f, "{t}")?;
                }
                Ok(())
            }
            Self::DuplicateColumnsFound { columns } => {
                writeln!(f, "Duplicate columns found:")?;
                for t in columns {
                    writeln!(f, "{t}")?;
                }
                Ok(())
            }
            Self::TableWithSchemaNotFound { name, schema } => {
                writeln!(f, "Table: {name} with schema: {schema} not found in SqlDoc")
            }
        }
    }
}

impl core::error::Error for DocError {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            #[cfg(feature = "std")]
            Self::FileReadError(e) => Some(e),
            Self::CommentError(e) => Some(e),
            Self::SqlParserError(e) => Some(e),
            Self::InvalidObjectName { .. }
            | Self::TableNotFound { .. }
            | Self::ColumnNotFound { .. }
            | Self::DuplicateTablesFound { .. }
            | Self::DuplicateColumnsFound { .. }
            | Self::TableWithSchemaNotFound { .. } => None,
        }
    }
}

#[cfg(feature = "std")]
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

    #[cfg(not(feature = "std"))]
    use alloc::{string::ToString, vec};

    use crate::{docs::TableDoc, error::DocError};

    #[cfg(feature = "std")]
    #[test]
    fn test_doc_errors() {
        use std::fs;

        use sqlparser::parser::ParserError;

        use crate::comments::{CommentError, Location};
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

    #[cfg(feature = "std")]
    #[test]
    fn test_doc_errors_from() {
        use std::fs;

        use sqlparser::parser::ParserError;

        use crate::comments::{CommentError, Location};

        let Err(io_error) = fs::read_dir("INVALID") else {
            panic!("there should not be a directory called INVALID")
        };
        let io_kind = io_error.kind();
        let doc_io_error: DocError = io_error.into();
        assert!(matches!(doc_io_error, DocError::FileReadError(_)));
        if let DocError::FileReadError(inner) = doc_io_error {
            assert_eq!(inner.kind(), io_kind);
        }

        let comment_error =
            CommentError::UnmatchedMultilineCommentStart { location: Location::default() };
        let comment_error_str = comment_error.to_string();
        let doc_comment_error: DocError = comment_error.into();
        assert!(matches!(doc_comment_error, DocError::CommentError(_)));
        if let DocError::CommentError(inner) = doc_comment_error {
            assert_eq!(inner.to_string(), comment_error_str);
        }

        let parser_error = ParserError::RecursionLimitExceeded;
        let parser_error_str = parser_error.to_string();
        let doc_parser_error: DocError = parser_error.into();
        assert!(matches!(doc_parser_error, DocError::SqlParserError(_)));
        if let DocError::SqlParserError(inner) = doc_parser_error {
            assert_eq!(inner.to_string(), parser_error_str);
        }
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_doc_error_source() {
        use std::{error::Error, fs};

        use sqlparser::parser::ParserError;

        use crate::comments::{CommentError, Location};

        let Err(io_err) = fs::read_dir("INVALID") else {
            panic!("there should not be a directory called INVALID")
        };
        let io_err_str = io_err.to_string();
        let doc_io = DocError::FileReadError(io_err);
        let src =
            doc_io.source().unwrap_or_else(|| panic!("expected Some(source) for FileReadError"));
        assert_eq!(src.to_string(), io_err_str);

        let comment =
            CommentError::UnmatchedMultilineCommentStart { location: Location::default() };
        let comment_str = comment.to_string();
        let doc_comment = DocError::CommentError(comment);
        let src = doc_comment
            .source()
            .unwrap_or_else(|| panic!("expected Some(source) for CommentError"));
        assert_eq!(src.to_string(), comment_str);

        let parser = ParserError::RecursionLimitExceeded;
        let parser_str = parser.to_string();
        let doc_parser = DocError::SqlParserError(parser);
        let src = doc_parser
            .source()
            .unwrap_or_else(|| panic!("expected Some(source) for SqlParserError"));
        assert_eq!(src.to_string(), parser_str);
    }

    #[cfg(feature = "std")]
    fn table_doc_for_test(name: &str) -> TableDoc {
        TableDoc::new(None, name.to_string(), None, vec![], None)
    }
    #[test]
    fn test_doc_error_display_invalid_object_name() {
        let e =
            DocError::InvalidObjectName { message: "bad object".to_string(), line: 12, column: 34 };
        assert_eq!(e.to_string(), "bad object at line 12, column 34");
    }

    #[test]
    fn test_doc_error_display_table_not_found() {
        let e = DocError::TableNotFound { name: "users".to_string() };
        assert_eq!(e.to_string(), "Table not found in SqlDoc: users");
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_doc_error_display_duplicate_tables_found() {
        let t1 = table_doc_for_test("dup_table");
        let t2 = table_doc_for_test("dup_table");
        let e = DocError::DuplicateTablesFound { tables: vec![t1, t2] };
        let s = e.to_string();
        assert!(s.contains("Duplicate tables found:"));
        assert!(s.contains("dup_table"), "output was: {s}");
    }

    #[test]
    fn test_doc_error_source_none_for_non_wrapped_variants() {
        use core::error::Error as _;
        let invalid = DocError::InvalidObjectName { message: "x".to_string(), line: 1, column: 1 };
        assert!(invalid.source().is_none());
        let not_found = DocError::TableNotFound { name: "x".to_string() };
        assert!(not_found.source().is_none());
        let dup = DocError::DuplicateTablesFound { tables: vec![] };
        assert!(dup.source().is_none());
    }

    #[test]
    fn test_doc_error_display_column_not_found() {
        let e = DocError::ColumnNotFound { name: "id".to_string() };
        assert_eq!(e.to_string(), "Column not found in TableDoc: id");
    }

    #[test]
    fn test_doc_error_display_duplicate_columns_found() {
        use crate::docs::ColumnDoc;

        let c1 = ColumnDoc::new("dup_col".to_string(), None);
        let c2 = ColumnDoc::new("dup_col".to_string(), None);
        let e = DocError::DuplicateColumnsFound { columns: vec![c1, c2] };

        let s = e.to_string();
        assert!(s.contains("Duplicate columns found:"), "output was: {s}");
        assert!(s.contains("dup_col"), "output was: {s}");
    }

    #[test]
    fn test_doc_error_source_none_for_column_variants() {
        use core::error::Error as _;

        let not_found = DocError::ColumnNotFound { name: "x".to_string() };
        assert!(not_found.source().is_none());

        let dup = DocError::DuplicateColumnsFound { columns: vec![] };
        assert!(dup.source().is_none());
    }

    #[test]
    fn test_doc_error_display_table_with_schema_not_found() {
        let e = DocError::TableWithSchemaNotFound {
            name: "events".to_string(),
            schema: "analytics".to_string(),
        };
        assert_eq!(e.to_string(), "Table: events with schema: analytics not found in SqlDoc\n");
    }

    #[test]
    fn test_doc_error_source_none_for_table_with_schema_not_found() {
        use core::error::Error as _;
        let e = DocError::TableWithSchemaNotFound {
            name: "events".to_string(),
            schema: "analytics".to_string(),
        };
        assert!(e.source().is_none());
    }
}
