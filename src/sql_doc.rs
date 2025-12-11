//! Module for the top level `SqlDoc` structure.

use std::path::{Path, PathBuf};

use crate::{
    ast::{ParsedSqlFile, ParsedSqlFileSet},
    comments::Comments,
    docs::{SqlFileDoc, TableDoc},
    error::DocError,
    files::{SqlFile, SqlFileSet},
};

/// Structure for Sql Documentation, built from [`TableDoc`] and
#[derive(Debug, Eq, PartialEq)]
pub struct SqlDoc {
    /// Holds the [`Vec`] of all tables found in all specified files.
    tables: Vec<TableDoc>,
    /// Optionally holds the [`Vec`] of each file's [`PathBuf`] and the original file's [`SqlFileDoc`]
    files: Option<Vec<(PathBuf, SqlFileDoc)>>,
}

/// Builder structure for the [`SqlDoc`]
pub struct SqlDocBuilder {
    /// The source for implementing the [`SqlDoc`] to be built
    source: SqlFileDocSource,
    /// The list of Paths to be ignored for parsing purposes.
    deny: Vec<String>,
    /// Used to indicate maintaining the `[(PathBuf, SqlFileDoc)]`
    retain_files: bool,
}

/// Enum for specifying a file doc source as a `directory` or a specific `file`
enum SqlFileDocSource {
    Dir(PathBuf),
    File(PathBuf),
}

impl SqlDoc {
    /// Method for creating a new [`SqlDoc`]
    #[must_use]
    pub const fn new(tables: Vec<TableDoc>, files: Option<Vec<(PathBuf, SqlFileDoc)>>) -> Self {
        Self { tables, files }
    }
    /// Method for generating builder from a directory.
    pub fn from_dir<P: AsRef<Path>>(root: P) -> SqlDocBuilder {
        SqlDocBuilder {
            source: SqlFileDocSource::Dir(root.as_ref().to_path_buf()),
            deny: Vec::new(),
            retain_files: false,
        }
    }
    /// Method for generating builder from a [`Path`] of a single file
    pub fn from_path<P: AsRef<Path>>(path: P) -> SqlDocBuilder {
        SqlDocBuilder {
            source: SqlFileDocSource::File(path.as_ref().to_path_buf()),
            deny: Vec::new(),
            retain_files: false,
        }
    }

    /// Method for finding a specific [`TableDoc`] by `name`
    ///
    /// # Parameters
    /// - the table `name` as a [`str`]
    ///
    /// # Errors
    /// - Will return [`DocError::TableNotFound`] if the expected table is not found
    /// - Will return [`DocError::DuplicateTablesFound`] if more than one table is found
    pub fn table(&self, table: &str) -> Result<&TableDoc, DocError> {
        let matches = self
            .tables
            .iter()
            .filter(|table_doc| table_doc.name() == table)
            .collect::<Vec<&TableDoc>>();
        match matches.as_slice() {
            [] => Err(DocError::TableNotFound { name: table.to_string() }),
            [only] => Ok(*only),
            _ => Err(DocError::DuplicateTablesFound {
                tables: matches.into_iter().cloned().collect(),
            }),
        }
    }

    /// Method for finding a specific [`TableDoc`] from `schema` and table `name`
    ///
    /// # Parameters
    /// - the table's `schema` as a [`str`]
    /// - the table's `name` as a [`str`]
    ///
    /// # Errors
    /// - Will return [`DocError::TableNotFound`] if the expected table is not found
    /// - Will return [`DocError::DuplicateTablesFound`] if more than one table is found
    pub fn table_with_schema(&self, schema: &str, name: &str) -> Result<&TableDoc, DocError> {
        let matches = self
            .tables
            .iter()
            .filter(|table_doc| table_doc.name() == name && table_doc.schema() == Some(schema))
            .collect::<Vec<&TableDoc>>();
        match matches.as_slice() {
            [] => Err(DocError::TableNotFound { name: name.to_string() }),
            [only] => Ok(*only),
            _ => Err(DocError::DuplicateTablesFound {
                tables: matches.into_iter().cloned().collect(),
            }),
        }
    }

    /// Getter method for returning the `&[TableDoc]`
    #[must_use]
    pub fn tables(&self) -> &[TableDoc] {
        &self.tables
    }
    /// Method to move tables out of Structure if needed
    #[must_use]
    pub fn into_tables(self) -> Vec<TableDoc> {
        self.tables
    }
    /// Getter method for returning the `Option<&[(PathBuf,SqlFileDoc)]>`
    #[must_use]
    pub fn files(&self) -> Option<&[(PathBuf, SqlFileDoc)]> {
        self.files.as_deref()
    }
}

impl SqlDocBuilder {
    /// Method for adding an item to the deny list
    ///
    /// # Parameters
    /// - The `path` that will be added to deny path `Vec`
    #[must_use]
    pub fn deny(mut self, deny_path: &str) -> Self {
        self.deny.push(deny_path.into());
        self
    }

    /// Setter that ticks on the option to retain the [`Vec<(PathBuf,SqlFileDoc)>`]
    #[must_use]
    pub const fn retain_files(mut self) -> Self {
        self.retain_files = true;
        self
    }

    /// Builds the [`SqlDoc`]
    ///
    /// # Errors
    /// - Will return `DocError` bubbled up
    pub fn build(self) -> Result<SqlDoc, DocError> {
        let docs: Vec<(PathBuf, SqlFileDoc)> = match &self.source {
            SqlFileDocSource::Dir(path) => generate_docs_from_dir(path, &self.deny)?,
            SqlFileDocSource::File(file) => {
                let gen_files = generate_docs_from_file(file)?;
                let (path, sql_doc) = gen_files;
                vec![(path, sql_doc)]
            }
        };
        let mut tables = Vec::new();
        if self.retain_files {
            let files = docs;
            for (_, sql_doc) in &files {
                tables.extend(sql_doc.tables().iter().cloned());
            }
            Ok(SqlDoc { tables, files: Some(files) })
        } else {
            for (_, sql_doc) in &docs {
                tables.extend(sql_doc.tables().iter().cloned());
            }
            Ok(SqlDoc { tables, files: None })
        }
    }
}

fn generate_docs_from_dir<P: AsRef<Path>, S: AsRef<str>>(
    source: P,
    deny: &[S],
) -> Result<Vec<(PathBuf, SqlFileDoc)>, DocError> {
    let deny_list: Vec<String> = deny.iter().map(|file| file.as_ref().to_string()).collect();
    let deny_option = if deny_list.is_empty() { None } else { Some(deny_list) };
    let file_set = SqlFileSet::new(source.as_ref(), deny_option)?;
    let parsed_files = ParsedSqlFileSet::parse_all(file_set)?;
    let mut sql_docs = Vec::new();
    for file in parsed_files.files() {
        let comments = Comments::parse_all_comments_from_file(file)?;
        let docs = SqlFileDoc::from_parsed_file(file, &comments)?;
        let path = file.file().path().to_path_buf();
        sql_docs.push((path, docs));
    }
    Ok(sql_docs)
}

fn generate_docs_from_file<P: AsRef<Path>>(source: P) -> Result<(PathBuf, SqlFileDoc), DocError> {
    let file = SqlFile::new(source.as_ref())?;
    let parsed_file = ParsedSqlFile::parse(file)?;
    let comments = Comments::parse_all_comments_from_file(&parsed_file)?;
    let docs = SqlFileDoc::from_parsed_file(&parsed_file, &comments)?;
    let path = parsed_file.file().path().to_path_buf();
    Ok((path, docs))
}

#[cfg(test)]
mod tests {
    use std::{env, fs, path::PathBuf};

    use crate::{
        SqlDoc,
        docs::{ColumnDoc, SqlFileDoc, TableDoc},
        error::DocError,
    };

    #[test]
    fn build_sql_doc_from_file() -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("build_sql_doc_from_file");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let file = base.join("test_file.sql");
        let sample = sample_sql();
        let (contents, expected): (Vec<_>, Vec<_>) = sample.into_iter().unzip();
        fs::write(&file, contents.join(""))?;
        let sql_doc = SqlDoc::from_path(&file).build()?;
        let doc = SqlDoc::new(expected.into_iter().flat_map(SqlDoc::into_tables).collect(), None);
        assert_eq!(sql_doc, doc);
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }
    #[test]
    fn build_sql_doc_from_dir() -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("build_sql_doc_from_dir");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let sample = sample_sql();
        let (contents, expected): (Vec<_>, Vec<_>) = sample.into_iter().unzip();
        for (idx, contents) in contents.iter().enumerate() {
            let path = base.join(format!("test_file{idx}.sql"));
            fs::write(&path, contents)?;
        }
        let sql_doc = SqlDoc::from_dir(&base).build()?;
        let mut actual: Vec<TableDoc> = sql_doc.into_tables();
        dbg!(&actual);
        let mut expected: Vec<TableDoc> =
            expected.into_iter().flat_map(SqlDoc::into_tables).collect();
        assert_eq!(actual.len(), expected.len());
        sort_tables(&mut actual);
        sort_tables(&mut expected);
        assert_eq!(actual, expected);
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }

    #[test]
    fn test_retrieve_table_and_schema() -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("build_sql_doc_with_schema");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let file = base.join("test_file.sql");
        let sample = sample_sql();
        let (contents, expected): (Vec<_>, Vec<_>) = sample.into_iter().unzip();
        fs::write(&file, contents.join(""))?;
        let sql_doc = SqlDoc::from_path(&file).build()?;
        let doc = SqlDoc::new(expected.into_iter().flat_map(SqlDoc::into_tables).collect(), None);
        assert_eq!(sql_doc, doc);
        let table = "users";
        let actual_table = sql_doc.table(table)?;
        let expected_table = doc.table(table)?;
        assert_eq!(actual_table, expected_table);
        let schema = "analytics";
        let schema_table = "events";
        let actual_schema = sql_doc.table_with_schema(schema, schema_table)?;
        let expected_schema = doc.table_with_schema(schema, schema_table)?;
        assert_eq!(actual_schema, expected_schema);
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }

    #[test]
    fn test_table_err() {
        let empty_set = SqlDoc::new(vec![], None);
        let empty_table_err = empty_set.table("name");
        assert!(empty_table_err.is_err());
        assert!(matches!(
            empty_table_err,
            Err(DocError::TableNotFound { name }) if name == "name"
        ));
        let duplicate_set = SqlDoc::new(
            vec![
                TableDoc::new(None, "duplicate".to_string(), None, vec![]),
                TableDoc::new(None, "duplicate".to_string(), None, vec![]),
            ],
            None,
        );
        let duplicate_tables_err = duplicate_set.table("duplicate");
        assert!(matches!(duplicate_tables_err, Err(DocError::DuplicateTablesFound { .. })));
    }

    #[test]
    fn test_schema_err() {
        let empty_set = SqlDoc::new(vec![], None);
        let empty_table_err = empty_set.table_with_schema("schema", "name");
        assert!(empty_table_err.is_err());
        assert!(matches!(
            empty_table_err,
            Err(DocError::TableNotFound { name }) if name == "name"
        ));
        let duplicate_set = SqlDoc::new(
            vec![
                TableDoc::new(Some("schema".to_string()), "duplicate".to_string(), None, vec![]),
                TableDoc::new(Some("schema".to_string()), "duplicate".to_string(), None, vec![]),
            ],
            None,
        );
        let duplicate_tables_err = duplicate_set.table_with_schema("schema", "duplicate");
        assert!(matches!(duplicate_tables_err, Err(DocError::DuplicateTablesFound { .. })));
    }
    fn sort_tables(tables: &mut [TableDoc]) {
        tables.sort_by(|a, b| {
            let a_key = (a.schema().unwrap_or(""), a.name());
            let b_key = (b.schema().unwrap_or(""), b.name());
            a_key.cmp(&b_key)
        });
    }

    fn sample_sql() -> Vec<(&'static str, SqlDoc)> {
        vec![
            (
                r"
            -- Users table
            CREATE TABLE users (
                -- id
                id INTEGER PRIMARY KEY,
                -- login name
                username TEXT NOT NULL
            );
            ",
                SqlDoc::new(
                    vec![TableDoc::new(
                        None,
                        "users".to_string(),
                        Some("Users table".to_string()),
                        vec![
                            ColumnDoc::new("id".to_string(), Some("id".to_string())),
                            ColumnDoc::new("username".to_string(), Some("login name".to_string())),
                        ],
                    )],
                    None,
                ),
            ),
            (
                r"
            /* Posts table */
            CREATE TABLE posts (
                /* primary key */
                id INTEGER PRIMARY KEY,
                title TEXT NOT NULL
            );
            ",
                SqlDoc::new(
                    vec![TableDoc::new(
                        None,
                        "posts".to_string(),
                        Some("Posts table".to_string()),
                        vec![
                            ColumnDoc::new("id".to_string(), Some("primary key".to_string())),
                            ColumnDoc::new("title".to_string(), None),
                        ],
                    )],
                    None,
                ),
            ),
            (
                r"
            CREATE TABLE things (
                id INTEGER PRIMARY KEY,
                name TEXT,
                value INTEGER
            );
            ",
                SqlDoc::new(
                    vec![TableDoc::new(
                        None,
                        "things".to_string(),
                        None,
                        vec![
                            ColumnDoc::new("id".to_string(), None),
                            ColumnDoc::new("name".to_string(), None),
                            ColumnDoc::new("value".to_string(), None),
                        ],
                    )],
                    None,
                ),
            ),
            (
                r"
            -- Table with schema
            CREATE TABLE analytics.events (
                /* event id */
                id INTEGER PRIMARY KEY,
                /* event payload */
                payload TEXT
            );
            ",
                SqlDoc::new(
                    vec![TableDoc::new(
                        Some("analytics".to_string()),
                        "events".to_string(),
                        Some("Table with schema".to_string()),
                        vec![
                            ColumnDoc::new("id".to_string(), Some("event id".to_string())),
                            ColumnDoc::new(
                                "payload".to_string(),
                                Some("event payload".to_string()),
                            ),
                        ],
                    )],
                    None,
                ),
            ),
        ]
    }

    #[test]
    fn test_sql_doc_getters() {
        let tables = vec![TableDoc::new(None, "name".to_string(), None, vec![])];
        let files = vec![(
            PathBuf::from("file.sql"),
            SqlFileDoc::new(vec![TableDoc::new(None, "name".to_string(), None, vec![])]),
        )];
        let sql_doc = SqlDoc::new(
            vec![TableDoc::new(None, "name".to_string(), None, vec![])],
            Some(vec![(
                PathBuf::from("file.sql"),
                SqlFileDoc::new(vec![TableDoc::new(None, "name".to_string(), None, vec![])]),
            )]),
        );
        assert_eq!(sql_doc.tables().len(), 1);
        assert_eq!(sql_doc.tables(), tables);

        assert_eq!(sql_doc.files().iter().len(), 1);
        assert_eq!(sql_doc.files(), Some(files).as_deref());
    }
}
