//! Module for parsing sql and comments and returning only comments connected to
//! statements
use sqlparser::ast::{Spanned, Statement};

use crate::{ast::ParsedSqlFile, comments::Comments};

/// Structure for containing the `name` of the `Column` and an [`Option`] for
/// the comment as a [`String`]
pub struct ColumnDoc {
    name: String,
    doc: Option<String>,
}
impl ColumnDoc {
    /// Creates a new [`ColumnDoc`]
    ///
    /// # Parameters
    /// - name: `String` - the name of the column
    /// - doc: `Option<String>` the comment for the column
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn new(name: String, doc: Option<String>) -> Self {
        Self { name, doc }
    }

    /// Getter for the `name` field
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Getter for the field `doc`
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn doc(&self) -> &Option<String> {
        &self.doc
    }
}

/// Structure for containing the `name` of the `Table`, an [`Option`] for the
/// comment as a [`String`], and a `Vec` of [`ColumnDoc`] contained in the table
pub struct TableDoc {
    name: String,
    doc: Option<String>,
    columns: Vec<ColumnDoc>,
}

impl TableDoc {
    /// Creates a new [`TableDoc`]
    ///
    /// # Parameters
    /// - name: `String` - the name of the table
    /// - doc: `Option<String>` of the comment for table
    /// - columns: the `Vec<ColumnDoc>` of all [`ColumnDoc`] for this table
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn new(name: String, doc: Option<String>, columns: Vec<ColumnDoc>) -> Self {
        Self { name, doc, columns }
    }

    /// Getter for the `name` field
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Getter for the `doc` field
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn doc(&self) -> &Option<String> {
        &self.doc
    }

    /// Getter for the `columns` field
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn columns(&self) -> &[ColumnDoc] {
        &self.columns
    }
}

/// Structure for containing the docs for every `Table` in an `.sql` file as a
/// `Vec` of [`TableDoc`]
pub struct SqlDocs {
    tables: Vec<TableDoc>,
}

impl SqlDocs {
    /// Create a new instance of [`SqlDocs`]
    #[must_use]
    pub const fn new(tables: Vec<TableDoc>) -> Self {
        Self { tables }
    }

    /// Builds documentation for sql file by attaching leading comments to
    /// tables and columns
    #[must_use]
    pub fn from_parsed_file(file: &ParsedSqlFile, comments: &Comments) -> Self {
        let mut tables = Vec::new();
        for statement in file.statements() {
            #[allow(clippy::single_match)]
            match statement {
                Statement::CreateTable(table) => {
                    let table_start_line = table.span().start.line;
                    let mut column_docs = Vec::new();
                    for column in &table.columns {
                        let column_start = column.span().start.line;
                        let column_leading = comments.leading_comment(column_start);
                        let column_name = column.name.value.clone();
                        let column_doc = match column_leading {
                            Some(col_comment) => {
                                ColumnDoc::new(column_name, Some(col_comment.text().to_string()))
                            }
                            None => ColumnDoc::new(column_name, None),
                        };
                        column_docs.push(column_doc);
                    }
                    let table_leading = comments.leading_comment(table_start_line);
                    let table_doc = match table_leading {
                        Some(comment) => {
                            TableDoc::new(
                                table.name.to_string(),
                                Some(comment.text().to_string()),
                                column_docs,
                            )
                        }
                        None => TableDoc::new(table.name.to_string(), None, column_docs),
                    };
                    tables.push(table_doc);
                }
                // can add support for other types of statements below
                _ => {}
            }
        }

        Self { tables }
    }

    /// Getter function to get a slice of [`TableDoc`]
    #[must_use]
    pub fn tables(&self) -> &[TableDoc] {
        &self.tables
    }
}
