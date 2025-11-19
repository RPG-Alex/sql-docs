//! The module for working with and loading the content from `sql` files
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

pub struct DenyList {
    deny_files: Vec<PathBuf>,
}

impl DenyList {
    pub fn new(files: Vec<String>) -> DenyList {
        let buf_files= files.iter().map(|f| PathBuf::from(f)).collect();
        DenyList { deny_files: buf_files }
    }
    pub fn deny_files(&self) -> &Vec<PathBuf> {
        &self.deny_files
    }
}

pub struct SqlFilesList {
    sql_files: Vec<PathBuf>,
}

impl SqlFilesList {
    pub fn new<P: AsRef<Path>>(path: P, deny_list: Option<Vec<String>>) -> io::Result<SqlFilesList> {
        let recursive_scan = recursive_dir_scan(path.as_ref())?;
        let allow_list = if let Some(list) = deny_list {
            let deny = DenyList::new(list);
            recursive_scan
                .into_iter()
                .filter(|p| !deny.deny_files().contains(p))
                .collect()
        } else {
            recursive_scan
        };

        Ok(SqlFilesList { sql_files: allow_list })
    }

    pub fn sql_files(&self) -> &[PathBuf] {
        &self.sql_files
    }
    pub fn sql_files_sorted(&self) -> Vec<&PathBuf> {
        let mut files: Vec<&PathBuf> = self.sql_files.iter().collect();
        files.sort();
        files
    }
}

/// Helper function to recursively scan the specified directory and collect all sql files found
fn recursive_dir_scan(path: &Path) -> io::Result<Vec<PathBuf>> {
    let mut sql_files = Vec::new();
    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() && path
        .extension()
        .and_then(|ext| ext.to_str())
        == Some("sql") {
            sql_files.push(path);
        } else if path.is_dir() {
            let nested = recursive_dir_scan(&path)?;
            sql_files.extend(nested);
        }
    }
    Ok(sql_files)
}

#[derive(Debug)]
pub struct SqlFile {
    path: PathBuf,
    content: String,
}
impl SqlFile {
     pub fn new(path: &Path) -> io::Result<SqlFile> {
        let content = fs::read_to_string(path)?;
        Ok(SqlFile { path: path.to_owned(), content })
     }
}

#[derive(Debug)]
pub struct SqlFileSet {
    files_contents: Vec<SqlFile>
}

impl SqlFileSet {
    pub fn new(path: &Path, deny_list: Option<Vec<String>>) -> io::Result<SqlFileSet> {
        let sql_files_list = SqlFilesList::new(path, deny_list)?;

        let files_contents = sql_files_list
            .sql_files()
            .iter()
            .map(|p| SqlFile::new(p))
            .collect::<io::Result<Vec<_>>>()?;

        Ok(SqlFileSet { files_contents })
    }
    pub fn iter(&self) -> impl Iterator<Item = &SqlFile> {
        self.files_contents.iter()
    } 
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use std::fs;

    #[test]
    fn test_recursive_scan_finds_only_sql_files_recursively() {
        // Create a unique temporary base directory
        let base = env::temp_dir().join("recursive_scan_test");
        // Clean up any previous leftovers
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap();

        // Create a nested subdirectory
        let sub = base.join("subdir");
        fs::create_dir_all(&sub).unwrap();

        // Paths for SQL files
        let file1 = base.join("one.sql");
        let file2 = sub.join("two.sql");

        // Non-SQL files (should be ignored)
        let non_sql1 = base.join("ignore.txt");
        let non_sql2 = sub.join("README.md");

        // Create the files to be tested
        fs::File::create(&file1).unwrap();
        fs::File::create(&file2).unwrap();
        fs::File::create(&non_sql1).unwrap();
        fs::File::create(&non_sql2).unwrap();

        // Call the function under test
        let mut found = recursive_dir_scan(base.as_path()).unwrap();

        // Sort so order doesn't matter
        found.sort();

        let mut expected = vec![file1, file2];
        expected.sort();

        assert_eq!(found, expected);

        // Optional cleanup
        let _ = fs::remove_dir_all(&base);
    }

    #[test]
    fn test_sql_file_list() {
        let base = env::temp_dir().join("recursive_scan_test2");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap();
        let sub = base.join("subdir");
        fs::create_dir_all(&sub).unwrap();
        let file1 = base.join("one.sql");
        let file2 = sub.join("two.sql");
        let non_sql1 = base.join("ignore.txt");
        let non_sql2 = sub.join("README.md");
        fs::File::create(&file1).unwrap();
        fs::File::create(&file2).unwrap();
        fs::File::create(&non_sql1).unwrap();
        fs::File::create(&non_sql2).unwrap();
        let sql_file_list = SqlFilesList::new(&base).unwrap();
        let mut expected = vec![&file1, &file2];
        expected.sort();
        assert_eq!(sql_file_list.sql_files_sorted(), expected);
        let _ = fs::remove_dir_all(&base);
    }
    #[test]
    fn test_sql_file_read() {
        let base = env::temp_dir().join("recursive_scan_test3");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base).unwrap();
        let sub = base.join("subdir");
        fs::create_dir_all(&sub).unwrap();
        let file1 = base.join("one.sql");
        let file2 = sub.join("two.sql");
        let non_sql1 = base.join("ignore.txt");
        let non_sql2 = sub.join("README.md");
        fs::File::create(&file1).unwrap();
        fs::File::create(&file2).unwrap();
        fs::File::create(&non_sql1).unwrap();
        fs::File::create(&non_sql2).unwrap();
        let sql_statement = "CREATE TABLE users( id INTEGER PRIMARY KEY);";
        fs::write(&file1, sql_statement).unwrap();
        fs::write(&file2, sql_statement).unwrap();
        let mut expected = vec![&file1, &file2];
        expected.sort();
        let mut found: Vec<&PathBuf> = Vec::new();
        let sql_file_set = SqlFileSet::new(&base).unwrap();
        for file in sql_file_set.iter() {
            assert_eq!(file.content, sql_statement);
            found.push(&file.path);
        }
        assert_eq!(found, expected);
        let _ = fs::remove_dir_all(&base);
    }

}
