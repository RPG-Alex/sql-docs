//! Module for structuring the Sql input

use alloc::string::String;

/// Holds the optional path and content of the `sql` to be parsed
#[derive(Debug)]
pub struct SqlSource {
    #[cfg(feature = "std")]
    path: Option<std::path::PathBuf>,
    content: String,
}

#[cfg(not(feature = "std"))]
impl From<String> for SqlSource {
    /// Creates an [`SqlSource`] from a a [`String`]
    fn from(content: String) -> Self {
        Self { content }
    }
}

#[cfg(feature = "std")]
impl From<String> for SqlSource {
    /// Creates an [`SqlSource`] from a a [`String`]
    fn from(content: String) -> Self {
        Self { content, path: None }
    }
}

#[cfg(feature = "std")]
impl SqlSource {
    /// Loads a [`SqlSource`] from the given path.
    ///
    /// # Errors
    /// - Returns an [`io::Error`] if the file cannot be read.
    pub fn from_path(path: &std::path::Path) -> std::io::Result<Self> {
        let source = crate::files::SqlContent::from_path(path)?;
        let content = source.content().to_owned();
        Ok(Self { path: Some(path.to_owned()), content })
    }

    /// Creates an [`SqlSource`] from a a [`String`] and a [`Option<PathBuf>`]
    #[must_use]
    pub const fn from_str_with_path(content: String, path: Option<std::path::PathBuf>) -> Self {
        Self { path, content }
    }

    /// Returns the filesystem path associated with this SQL file.
    #[must_use]
    pub fn path(&self) -> Option<&std::path::Path> {
        self.path.as_deref()
    }
    /// Returns the [`PathBuf`] for the current path
    #[must_use]
    pub fn path_into_path_buf(&self) -> Option<std::path::PathBuf> {
        self.path.clone()
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
    #[cfg(feature = "std")]
    pub fn sql_sources(path: &std::path::Path, deny_list: &[String]) -> std::io::Result<Vec<Self>> {
        let sql_content_set = crate::files::SqlContentSet::new(path, deny_list)?;

        let files_contents = sql_content_set
            .iter()
            .map(|p| Self::from_path(p.path()))
            .collect::<std::io::Result<Vec<_>>>()?;

        Ok(files_contents)
    }
}

impl SqlSource {
    /// Returns the raw SQL text contained in this file.
    #[must_use]
    pub fn content(&self) -> &str {
        &self.content
    }
}

#[cfg(test)]
mod tests {
    use crate::source::SqlSource;

    #[cfg(feature = "std")]
    #[test]
    fn test_sql_file_read() -> Result<(), Box<dyn std::error::Error>> {
        use std::{env, fs, path::PathBuf};
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

    #[cfg(not(feature = "std"))]
    #[test]
    fn test_sql_file_new_from_str_has_no_path_and_preserves_content() {
        use alloc::boxed::Box;
        use alloc::vec;
        use alloc::vec::Vec;
        let sql = "SELECT * FROM users;";
        let file = SqlSource::from_str(sql.to_owned(), None);
        assert!(file.path().is_none());
        assert!(file.path_into_path_buf().is_none());
        assert_eq!(file.content(), sql);
    }
}
