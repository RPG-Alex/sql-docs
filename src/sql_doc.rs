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
    /// Used to indicate maintaining the [`Vec<(PathBuf, SqlFileDoc`]
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
    pub fn deny(mut self, deny_path: &str) -> Self {
        self.deny.push(deny_path.into());
        self
    }

    /// Setter that ticks on the option to retain the [`Vec<(PathBuf,SqlFileDoc)>`]
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
