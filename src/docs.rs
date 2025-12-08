//! Module for parsing sql and comments and returning `table` and `column`
//! information, including comments
use sqlparser::ast::{Spanned, Statement};

use crate::{ast::ParsedSqlFile, comments::Comments};

/// Structure for containing the `name` of the `Column` and an [`Option`] for
/// the comment as a [`String`]
#[derive(Clone, Debug, Eq, PartialEq)]
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

/// Structure for containing the `name` of the `Table`, an [`Option`] for if the
/// table has a schema,  an [`Option`] for the comment as a [`String`], and a 
/// `Vec` of [`ColumnDoc`] contained in the table
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TableDoc {
    name: String,
    schema: Option<String>,
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
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SqlDocs {
    tables: Vec<TableDoc>,
}

impl SqlDocs {
    /// Create a new instance of [`SqlDocs`]
    ///
    /// # Parameters
    /// - `tables` the `Vec` of [`TableDoc`] for the struct
    #[must_use]
    pub const fn new(tables: Vec<TableDoc>) -> Self {
        Self { tables }
    }

    /// Final structured documentation extracted from one SQL file.
    ///
    /// This merges:
    /// - Parsed SQL AST (`CREATE TABLE` statements for example)
    /// - Comment spans into a format suitable for documentation generation.
    ///
    /// # Parameters
    /// - `file`: the [`ParsedSqlFile`]
    /// - `comments`: the parsed [`Comments`]
    #[must_use]
    pub fn from_parsed_file(file: &ParsedSqlFile, comments: &Comments) -> Self {
        let mut tables = Vec::new();
        for statement in file.statements() {
            #[allow(clippy::single_match)]
            match statement {
                Statement::CreateTable(table) => {
                    let table_start = table.span().start.line;
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
                    let table_leading = comments.leading_comment(table_start);
                    let table_doc = match table_leading {
                        Some(comment) => TableDoc::new(
                            table.name.to_string(),
                            Some(comment.text().to_string()),
                            column_docs,
                        ),
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




#[cfg(test)]
mod tests {
    use crate::docs::{ColumnDoc, SqlDocs, TableDoc};

    #[test]
    fn test_sql_docs_struct() {
        let column_doc = ColumnDoc::new("id".to_string(), Some("The ID for the table".to_string()));
        let columns = vec![column_doc];
        let table_doc =
            TableDoc::new("user".to_string(), Some("The table for users".to_string()), columns);
        let tables = vec![table_doc];
        let sql_doc = SqlDocs::new(tables);
        let sql_doc_val =
            sql_doc.tables().first().map_or_else(|| panic!("unable to find tables"), |val| val);
        assert_eq!(sql_doc_val.name(), "user");
        let sql_doc_val_column = sql_doc_val
            .columns()
            .first()
            .map_or_else(|| panic!("unable to find columns"), |val| val);
        assert_eq!(sql_doc_val_column.name(), "id");
    }

    #[test]
    fn generate_docs_files() -> Result<(), Box<dyn std::error::Error>> {
        use crate::{ast::ParsedSqlFileSet, comments::Comments, files::SqlFileSet};
        use std::path::Path;
        let path = Path::new("sql_files");
        let set = SqlFileSet::new(path, None)?;
        let parsed_set = ParsedSqlFileSet::parse_all(set)?;
        let expected_values = expect_values();

        for file in parsed_set.files() {
            let comments = Comments::parse_all_comments_from_file(file)?;
            let docs = SqlDocs::from_parsed_file(file, &comments);
            let filename = file
                .file()
                .path()
                .file_name()
                .and_then(|s| s.to_str())
                .map_or_else(|| panic!("unable to find file name"), |val| val);

            match filename {
                "with_single_line_comments.sql" | "with_mixed_comments.sql" => {
                    assert_eq!(&docs, &expected_values[0]);
                }
                "with_multiline_comments.sql" => {
                    assert_eq!(&docs, &expected_values[1]);
                }
                "without_comments.sql" => {
                    let expected = expected_without_comments_docs();
                    assert_eq!(&docs, &expected);
                }
                other => {
                    unreachable!(
                        "unexpected test file {other}; directory should only contain known test files"
                    );
                }
            }
        }

        Ok(())
    }

    fn expected_without_comments_docs() -> SqlDocs {
        SqlDocs::new(vec![
            TableDoc::new(
                "users".to_string(),
                None,
                vec![
                    ColumnDoc::new("id".to_string(), None),
                    ColumnDoc::new("username".to_string(), None),
                    ColumnDoc::new("email".to_string(), None),
                    ColumnDoc::new("created_at".to_string(), None),
                ],
            ),
            TableDoc::new(
                "posts".to_string(),
                None,
                vec![
                    ColumnDoc::new("id".to_string(), None),
                    ColumnDoc::new("title".to_string(), None),
                    ColumnDoc::new("user_id".to_string(), None),
                    ColumnDoc::new("body".to_string(), None),
                    ColumnDoc::new("published_at".to_string(), None),
                ],
            ),
        ])
    }

    fn expect_values() -> Vec<SqlDocs> {
        let mut docs = Vec::new();

        let first_docs = SqlDocs::new(vec![
            TableDoc::new(
                "users".to_string(),
                Some("Users table stores user account information".to_string()),
                vec![
                    ColumnDoc::new("id".to_string(), Some("Primary key".to_string())),
                    ColumnDoc::new("username".to_string(), Some("Username for login".to_string())),
                    ColumnDoc::new("email".to_string(), Some("Email address".to_string())),
                    ColumnDoc::new(
                        "created_at".to_string(),
                        Some("When the user registered".to_string()),
                    ),
                ],
            ),
            TableDoc::new(
                "posts".to_string(),
                Some("Posts table stores blog posts".to_string()),
                vec![
                    ColumnDoc::new("id".to_string(), Some("Primary key".to_string())),
                    ColumnDoc::new("title".to_string(), Some("Post title".to_string())),
                    ColumnDoc::new(
                        "user_id".to_string(),
                        Some("Foreign key linking to users".to_string()),
                    ),
                    ColumnDoc::new("body".to_string(), Some("Main body text".to_string())),
                    ColumnDoc::new(
                        "published_at".to_string(),
                        Some("When the post was created".to_string()),
                    ),
                ],
            ),
        ]);
        docs.push(first_docs);

        let second_docs = SqlDocs::new(vec![
            TableDoc::new(
                "users".to_string(),
                Some("Users table stores user account information\nmultiline".to_string()),
                vec![
                    ColumnDoc::new("id".to_string(), Some("Primary key\nmultiline".to_string())),
                    ColumnDoc::new(
                        "username".to_string(),
                        Some("Username for login\nmultiline".to_string()),
                    ),
                    ColumnDoc::new(
                        "email".to_string(),
                        Some("Email address\nmultiline".to_string()),
                    ),
                    ColumnDoc::new(
                        "created_at".to_string(),
                        Some("When the user registered\nmultiline".to_string()),
                    ),
                ],
            ),
            TableDoc::new(
                "posts".to_string(),
                Some("Posts table stores blog posts\nmultiline".to_string()),
                vec![
                    ColumnDoc::new("id".to_string(), Some("Primary key\nmultiline".to_string())),
                    ColumnDoc::new("title".to_string(), Some("Post title\nmultiline".to_string())),
                    ColumnDoc::new(
                        "user_id".to_string(),
                        Some("Foreign key linking to users\nmultiline".to_string()),
                    ),
                    ColumnDoc::new(
                        "body".to_string(),
                        Some("Main body text\nmultiline".to_string()),
                    ),
                    ColumnDoc::new(
                        "published_at".to_string(),
                        Some("When the post was created\nmultiline".to_string()),
                    ),
                ],
            ),
        ]);
        docs.push(second_docs);

        docs
    }
}
