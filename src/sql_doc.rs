//! Module for the top level `SqlDoc` structure. 

use std::path::{Path, PathBuf};

use crate::{
    docs::{SqlFileDoc, TableDoc},
    error::DocError,
};

/// Structure for Sql Documentation, built from [`TableDoc`] and 
pub struct SqlDoc {
    /// Holds the [`Vec`] of all tables found in all specified files.
    tables: Vec<TableDoc>,
    /// Holds the [`Vec`] of each file's [`PathBuf`] and the original file's [`SqlFileDoc`]
    files: Vec<(PathBuf, SqlFileDoc)>,
}


/// Builder structure for the [`SqlDoc`]
pub struct SqlDocBuilder {
    /// The source for implementing the [`SqlDoc`] to be built
    source: SqlFileDocSource,
    /// The list of files to be ignored for parsing purposes. 
    deny: Vec<String>,
}

/// Enum for specifying a file doc source as a `directory` or a specific `file`
enum SqlFileDocSource {
    Dir(PathBuf),
    File(PathBuf),
}

impl SqlDoc {
    /// Method for generating builder from a directory. 
    pub fn from_dir<P: AsRef<Path>>(root: P) -> SqlDocBuilder {
        SqlDocBuilder { source: SqlFileDocSource::Dir(root.as_ref().to_path_buf()), deny: Vec::new() }
    }
    /// Method for generating builder from a [`Path`] of a single file 
    pub fn from_path<P: AsRef<Path>>(path: P) -> SqlDocBuilder {
        SqlDocBuilder { source: SqlFileDocSource::File(path.as_ref().to_path_buf()), deny: Vec::new() }
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
        let matches = self.tables.iter().filter(|table_doc| table_doc.name() == table).collect::<Vec<&TableDoc>>();
        match matches.as_slice() {
            [] => Err(DocError::TableNotFound {
                name: table.to_string(),
            }),
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
        let matches = self.tables.iter().filter(|table_doc| table_doc.name() == name && table_doc.schema() == Some(schema)).collect::<Vec<&TableDoc>>();
        match matches.as_slice() {
            [] => Err(DocError::TableNotFound {
                name: name.to_string(),
            }),
            [only] => Ok(*only),
            _ => Err(DocError::DuplicateTablesFound {
                tables: matches.into_iter().cloned().collect(),
            }),
        }
    }
}

impl SqlDocBuilder {

}
