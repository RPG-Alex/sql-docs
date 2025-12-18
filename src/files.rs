//! Module that offers utilities for discovering, filtering, and loading `.sql`
//! files from the filesystem.

use std::{
    fs, io,
    path::{Path, PathBuf},
};

/// A list of SQL files that should be excluded from processing.
///
/// Entries in the deny list are treated as full [`PathBuf`] paths.
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
pub struct SqlFilesList {
    sql_files: Vec<PathBuf>,
}

impl SqlFilesList {
    /// Creates a list of `.sql` files under `path`, optionally excluding files
    /// whose full paths appear in `deny_list`.
    ///
    /// Paths in `deny_list` must match exactly the `PathBuf` returned by
    /// recursion. For convenience, the top-level API accepts `&[&str]`
    /// instead.
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
        let allow_list = {
            let deny = DenyList::new(deny_list);
            recursive_scan.into_iter().filter(|p| !deny.deny_files().contains(p)).collect()
        };

        Ok(Self { sql_files: allow_list })
    }

    /// Returns the discovered `.sql` files in their original order.
    #[must_use]
    pub fn sql_files(&self) -> &[PathBuf] {
        &self.sql_files
    }

    /// Returns a sorted view of the discovered `.sql` files.
    #[must_use]
    pub fn sql_files_sorted(&self) -> Vec<&PathBuf> {
        let mut files: Vec<&PathBuf> = self.sql_files.iter().collect();
        files.sort();
        files
    }
}

impl From<SqlFilesList> for Vec<PathBuf> {
    fn from(value: SqlFilesList) -> Self {
        value.sql_files
    }
}

/// Recursively scans the directory for `.sql` files.
///
/// This function:
/// - Walks subdirectories
/// - Collects *only* `.sql` files
/// - Returns paths in discovery order
///
/// This does **not** load file contents. See [`SqlFile`] for that.
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

/// A single `.sql` file and its loaded contents.
#[derive(Debug)]
pub struct SqlFile {
    path: PathBuf,
    content: String,
}

impl SqlFile {
    /// Loads a [`SqlFile`] from the given path.
    ///
    /// This reads the entire file contents into memory as a [`String`].
    ///
    /// # Errors
    ///
    /// Returns an [`io::Error`] if the file cannot be read.
    pub fn new(path: &Path) -> io::Result<Self> {
        let content = fs::read_to_string(path)?;
        Ok(Self { path: path.to_owned(), content })
    }

    /// Returns the filesystem path associated with this SQL file.
    #[must_use]
    pub fn path(&self) -> &Path {
        &self.path
    }
    /// Returns the [`PathBuf`] for the current path
    #[must_use]
    pub fn path_into_path_buf(&self) -> PathBuf {
        self.path.clone()
    }

    /// Returns the raw SQL text contained in this file.
    #[must_use]
    pub fn content(&self) -> &str {
        &self.content
    }
}

/// A loaded set of `.sql` files, including their contents.
#[derive(Debug)]
pub struct SqlFileSet {
    files_contents: Vec<SqlFile>,
}

impl SqlFileSet {
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
        let sql_files_list = SqlFilesList::new(path, deny_list)?;

        let files_contents = sql_files_list
            .sql_files()
            .iter()
            .map(|p| SqlFile::new(p))
            .collect::<io::Result<Vec<_>>>()?;

        Ok(Self { files_contents })
    }

    /// Returns an iterator over the loaded SQL files in this set.
    pub fn iter(&self) -> impl Iterator<Item = &SqlFile> {
        self.files_contents.iter()
    }
}

impl IntoIterator for SqlFileSet {
    type IntoIter = std::vec::IntoIter<SqlFile>;
    type Item = SqlFile;

    fn into_iter(self) -> Self::IntoIter {
        self.files_contents.into_iter()
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
        let sql_file_list = SqlFilesList::new(&base, &[])?;
        let mut expected = vec![&file1, &file2];
        expected.sort();
        assert_eq!(sql_file_list.sql_files_sorted(), expected);
        let _ = fs::remove_dir_all(&base);

        Ok(())
    }
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
        let sql_file_set = SqlFileSet::new(&base, &[])?;
        for file in sql_file_set.iter() {
            assert_eq!(file.content, sql_statement);
            found.push(&file.path);
        }
        assert_eq!(found, expected);
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
        let sql_file_list = SqlFilesList::new(&base, deny_list)?;
        let found = sql_file_list.sql_files_sorted();
        let mut expected = vec![&file2];
        expected.sort();
        assert_eq!(found, expected);
        let _ = fs::remove_dir_all(&base);

        Ok(())
    }

    #[test]
    fn test_file_fails() {
        let invalid_dir = "INVALID";
        let failed_list = SqlFilesList::new(invalid_dir, &[]);
        assert!(failed_list.is_err());

        let base = env::temp_dir().join("test_files_fails_dir");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap_or_else(|e| panic!("panicked on {e}"));
        let bad_file = base.join("one.sql");
        fs::File::create(&bad_file).unwrap_or_else(|e| panic!("panicked on {e}"));
        let failed_read = SqlFilesList::new(&bad_file, &[]);
        assert!(failed_read.is_err());
        let missed_file = base.join("missing.sql");
        let missing_file = SqlFile::new(&missed_file);
        assert!(missing_file.is_err());
        let _ = fs::remove_dir_all(&base);

        let bad_file_list = SqlFileSet::new(Path::new(invalid_dir), &[]);
        assert!(bad_file_list.is_err());
    }
}
