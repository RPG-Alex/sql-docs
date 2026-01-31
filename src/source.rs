//! Module for structuring the Sql input
use crate::files::{SqlContent, SqlContentSet};
use std::{
    io,
    path::{Path, PathBuf},
};

/// Holds the optional path and content of the `sql` to be parsed
#[derive(Debug)]
pub struct SqlSource {
    path: Option<PathBuf>,
    content: String,
}

impl SqlSource {
    /// Loads a [`SqlSource`] from the given path.
    ///
    /// # Errors
    /// - Returns an [`io::Error`] if the file cannot be read.
    pub fn from_path(path: &Path) -> io::Result<Self> {
        let source = SqlContent::from_path(path)?;
        let content = source.content().to_owned();
        Ok(Self { path: Some(path.to_owned()), content })
    }

    /// Creates an [`SqlSource`] from a a [`String`] and a [`Option<PathBuf>`]
    #[must_use]
    pub const fn from_str(content: String, path: Option<PathBuf>) -> Self {
        Self { path, content }
    }

    /// Returns the filesystem path associated with this SQL file.
    #[must_use]
    pub fn path(&self) -> Option<&Path> {
        self.path.as_deref()
    }
    /// Returns the [`PathBuf`] for the current path
    #[must_use]
    pub fn path_into_path_buf(&self) -> Option<PathBuf> {
        self.path.clone()
    }

    /// Returns the raw SQL text contained in this file.
    #[must_use]
    pub fn content(&self) -> &str {
        &self.content
    }

    /// Recursively discovers `.sql` files under `path`, applies the optional
    /// deny list, and loads the contents of each file.
    ///
    /// # Parameters
    ///
    /// - `path`: Root directory to scan recursively for `.sql` files.
    /// - `deny_list`: Optional list of path-like strings representing files
    ///
    /// # Errors
    ///
    /// Returns an [`io::Error`] if directory traversal fails or if any of the
    /// discovered SQL files cannot be read.
    pub fn sql_sources(path: &Path, deny_list: &[String]) -> io::Result<Vec<Self>> {
        let sql_content_set = SqlContentSet::new(path, deny_list)?;

        let files_contents = sql_content_set
            .iter()
            .map(|p| Self::from_path(p.path()))
            .collect::<io::Result<Vec<_>>>()?;

        Ok(files_contents)
    }
}

#[cfg(test)]
mod tests {
    use std::{env, fs, path::PathBuf};

    use crate::source::SqlSource;

    #[test]
    fn test_sql_file_read() -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("recursive_scan_test3");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let sub = base.join("subdir");
        fs::create_dir_all(&sub)?;
        let file1 = base.join("one.sql");
        let file2 = sub.join("two.sql");
        let non_sql1 = base.join("ignore.txt");
        let non_sql2 = sub.join("README.md");
        fs::File::create(&file1)?;
        fs::File::create(&file2)?;
        fs::File::create(&non_sql1)?;
        fs::File::create(&non_sql2)?;
        let sql_statement = "CREATE TABLE users( id INTEGER PRIMARY KEY);";
        fs::write(&file1, sql_statement)?;
        fs::write(&file2, sql_statement)?;
        let mut expected = vec![&file1, &file2];
        expected.sort();
        let mut found: Vec<&PathBuf> = Vec::new();
        let sql_file_set = SqlSource::sql_sources(&base, &[])?;
        for file in &sql_file_set {
            assert_eq!(file.content, sql_statement);
            if let Some(p) = &file.path {
                found.push(p);
            }
        }
        assert_eq!(found, expected);
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }

    #[test]
    fn test_sql_file_new_from_str_has_no_path_and_preserves_content() {
        let sql = "SELECT * FROM users;";
        let file = SqlSource::from_str(sql.to_owned(), None);
        assert!(file.path().is_none());
        assert!(file.path_into_path_buf().is_none());
        assert_eq!(file.content(), sql);
    }
}
