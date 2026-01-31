//! Discover and filter `.sql` files on disk.
//!
//! This module finds file paths; it does not parse SQL or extract comments.

use std::{
    fs, io,
    path::{Path, PathBuf},
};

/// A list of SQL files to exclude from processing.
///
/// Entries in the deny list are treated as full [`PathBuf`] paths.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DenyList {
    deny_files: Vec<PathBuf>,
}

impl DenyList {
    /// Creates a new [`DenyList`] from a list of path-like strings.
    pub fn new(files: &[String]) -> Self {
        let buf_files = files.iter().map(PathBuf::from).collect();
        Self { deny_files: buf_files }
    }

    /// Returns the underlying collection of denied file paths.
    #[must_use]
    pub fn deny_files(&self) -> &[PathBuf] {
        &self.deny_files
    }
}

/// A collection of discovered `.sql` files under a given directory.
#[derive(Debug)]
pub struct SqlFiles {
    sql_files: Vec<PathBuf>,
}

impl SqlFiles {
    /// Creates a list of `.sql` files under `path`, and optionally excludes files
    /// that are in the `deny_list`.
    ///
    /// Deny entries must match the discovered full path exactly (string equality on
    /// the resulting [`PathBuf`]).
    ///
    /// # Parameters
    ///
    /// - `path`: any type that implements [`AsRef<Path>`].
    /// - `deny_list`: optional list of path-like strings representing files to
    ///   exclude.
    ///
    /// # Errors
    ///
    /// Returns an [`io::Error`] if directory traversal fails.
    pub fn new<P: AsRef<Path>>(path: P, deny_list: &[String]) -> io::Result<Self> {
        let recursive_scan = recursive_dir_scan(path.as_ref())?;
        let mut allow_list: Vec<PathBuf> = {
            let deny = DenyList::new(deny_list);
            recursive_scan.into_iter().filter(|p| !deny.deny_files().contains(p)).collect()
        };
        allow_list.sort();
        Ok(Self { sql_files: allow_list })
    }

    /// Returns discovered `.sql` files in discovery order (filesystem-dependent).
    #[must_use]
    pub fn sql_files(&self) -> Vec<PathBuf> {
        let mut files: Vec<PathBuf> =
            self.sql_files.iter().map(std::borrow::ToOwned::to_owned).collect();
        files.sort();
        files
    }
}

/// Adds `.sql` files to a Vec of [`PathBuf`] recursively.
fn recursive_dir_scan(path: &Path) -> io::Result<Vec<PathBuf>> {
    let mut sql_files = Vec::new();
    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() && path.extension().and_then(|ext| ext.to_str()) == Some("sql") {
            sql_files.push(path);
        } else if path.is_dir() {
            let nested = recursive_dir_scan(&path)?;
            sql_files.extend(nested);
        }
    }
    Ok(sql_files)
}

impl From<SqlFiles> for Vec<PathBuf> {
    fn from(value: SqlFiles) -> Self {
        value.sql_files
    }
}

/// Holds the path and retrieved file content
pub struct SqlContent {
    path: PathBuf,
    content: String,
}

impl SqlContent {
    /// Creates structure from a [`PathBuf`]
    ///
    /// # Errors
    /// - Will return [`io::Error`] if the file is not able to be parsed
    pub fn from_path(path: &Path) -> io::Result<Self> {
        let content = fs::read_to_string(path)?;
        Ok(Self { path: path.to_path_buf(), content })
    }

    /// Returns the [`PathBuf`] as a reference
    #[must_use]
    pub const fn path(&self) -> &PathBuf {
        &self.path
    }

    /// Returns the [`str`] as reference of file content
    #[must_use]
    pub fn content(&self) -> &str {
        &self.content
    }
}

/// Wrapper for `Vec` of [`SqlContent`]
pub struct SqlContentSet {
    sql_content: Vec<SqlContent>,
}

impl SqlContentSet {
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
    pub fn new(path: &Path, deny_list: &[String]) -> io::Result<Self> {
        let sql_files_list = SqlFiles::new(path, deny_list)?;

        let sql_content = sql_files_list
            .sql_files()
            .iter()
            .map(|p| SqlContent::from_path(p))
            .collect::<io::Result<Vec<_>>>()?;

        Ok(Self { sql_content })
    }

    /// Returns an iterator over the loaded SQL files in this set.
    pub fn iter(&self) -> impl Iterator<Item = &SqlContent> {
        self.sql_content.iter()
    }
}

#[cfg(test)]
mod tests {
    use std::{env, fs};

    use super::*;

    #[test]
    fn test_recursive_scan_finds_only_sql_files_recursively()
    -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("recursive_scan_test_sql");
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
        let mut found = recursive_dir_scan(base.as_path())?;
        found.sort();
        let mut expected = vec![file1, file2];
        expected.sort();
        assert_eq!(found, expected);
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }

    #[test]
    fn test_recursive_dir_scan_errs() -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("recursive_scan_test");
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

        let bad_path = Path::new("bad_path");
        let bad_dir_scan = recursive_dir_scan(bad_path);
        assert!(bad_dir_scan.is_err());

        let _ = fs::remove_dir_all(&base);
        Ok(())
    }

    #[test]
    fn test_sql_file_list() -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("recursive_scan_test2");
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
        let sql_file_list = SqlFiles::new(&base, &[])?;
        let mut expected = vec![file1, file2];
        expected.sort();
        assert_eq!(sql_file_list.sql_files(), expected);
        let _ = fs::remove_dir_all(&base);

        Ok(())
    }

    #[test]
    fn test_deny_list_filters_full_paths() -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("deny_list_full_path_test");
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
        let deny_list = &[file1.to_string_lossy().to_string()];
        let sql_file_list = SqlFiles::new(&base, deny_list)?;
        let found = sql_file_list.sql_files();
        let mut expected = vec![file2];
        expected.sort();
        assert_eq!(found, expected);
        let _ = fs::remove_dir_all(&base);

        Ok(())
    }

    #[test]
    fn test_file_fails() {
        let invalid_dir = "INVALID";
        let failed_list = SqlFiles::new(invalid_dir, &[]);
        assert!(failed_list.is_err());

        let base = env::temp_dir().join("test_files_fails_dir");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap_or_else(|e| panic!("panicked on {e}"));
        let bad_file = base.join("one.sql");
        fs::File::create(&bad_file).unwrap_or_else(|e| panic!("panicked on {e}"));
        let failed_read = SqlFiles::new(&bad_file, &[]);
        assert!(failed_read.is_err());
        let missed_file = base.join("missing.sql");
        let missing_file = SqlContent::from_path(missed_file.as_path());
        assert!(missing_file.is_err());
        let _ = fs::remove_dir_all(&base);

        let bad_file_list = SqlContentSet::new(Path::new(invalid_dir), &[]);
        assert!(bad_file_list.is_err());
    }
    #[test]
    fn test_from_sql_files_list_into_vec_pathbuf_preserves_contents_and_order()
    -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("from_impl_sql_files_list_test");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let file1 = base.join("one.sql");
        let file2 = base.join("two.sql");
        let noise = base.join("ignore.txt");
        fs::File::create(&file1)?;
        fs::File::create(&file2)?;
        fs::File::create(&noise)?;
        let sql_file_list = SqlFiles::new(&base, &[])?;
        let expected: Vec<PathBuf> = sql_file_list.sql_files();
        let got: Vec<PathBuf> = Vec::from(sql_file_list);
        assert_eq!(got, expected);
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }
}
