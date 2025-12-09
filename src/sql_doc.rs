//! Module for the top level `SqlDoc` structure. 

use std::path::{Path, PathBuf};

use crate::{
    ast::ParsedSqlFileSet,
    comments::Comments,
    docs::{SqlFileDoc, TableDoc, ColumnDoc},
    error::DocError,
    files::SqlFileSet,
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


}

impl SqlDocBuilder {

}
