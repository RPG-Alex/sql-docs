//! Parse SQL text into an AST (`sqlparser`) for downstream comment attachment.
//!
//! This module does not interpret semantics; it only produces an AST + file metadata.

use std::path::{Path, PathBuf};

use sqlparser::{
    ast::Statement,
    dialect::GenericDialect,
    parser::{Parser, ParserError},
};

use crate::files::{SqlFile, SqlFileSet};

/// A single SQL file plus all [`Statement`].
#[derive(Debug)]
pub struct ParsedSqlFile {
    file: SqlFile,
    statements: Vec<Statement>,
}

impl ParsedSqlFile {
    /// Parses a [`SqlFile`] into `sqlparser` [`Statement`] nodes.
    ///
    /// This is the AST layer used by the `comments` module to attach leading
    /// comment spans to statements/columns.
    ///
    /// # Parameters
    /// - `file`: the [`SqlFile`] to parse
    ///
    /// # Errors
    /// - Returns [`ParserError`] if parsing fails
    pub fn parse(file: SqlFile) -> Result<Self, ParserError> {
        let dialect = GenericDialect {};
        let statements = Parser::parse_sql(&dialect, file.content())?;
        Ok(Self { file, statements })
    }

    /// Getter method for returning the [`SqlFile`]
    #[must_use]
    pub const fn file(&self) -> &SqlFile {
        &self.file
    }

    /// Getter method for returning the current object's file's path
    #[must_use]
    pub fn path(&self) -> Option<&Path> {
        self.file.path()
    }

    /// Getter that returns an [`PathBuf`] for the path rather than `&Path`
    #[must_use]
    pub fn path_into_path_buf(&self) -> Option<PathBuf> {
        self.file.path_into_path_buf()
    }

    /// Getter for the file's content
    #[must_use]
    pub fn content(&self) -> &str {
        self.file.content()
    }

    /// Getter method for returning the vector of all statements [`Statement`]
    #[must_use]
    pub fn statements(&self) -> &[Statement] {
        &self.statements
    }
}

/// Struct to contain the vector of parsed SQL files
#[derive(Debug)]
pub struct ParsedSqlFileSet {
    files: Vec<ParsedSqlFile>,
}

impl ParsedSqlFileSet {
    /// Method that parses all members in a [`SqlFileSet`]
    ///
    /// # Parameters
    /// - `set` the set of [`SqlFileSet`]
    ///
    /// # Errors
    /// - [`ParserError`] is returned for any errors parsing
    pub fn parse_all(set: SqlFileSet) -> Result<Self, ParserError> {
        let files = set.into_iter().map(ParsedSqlFile::parse).collect::<Result<Vec<_>, _>>()?;

        Ok(Self { files })
    }

    /// Getter method for returning the vector of all [`ParsedSqlFile`]
    #[must_use]
    pub fn files(&self) -> &[ParsedSqlFile] {
        &self.files
    }
}

#[cfg(test)]
mod tests {
    use std::{env, fs};

    use super::*;
    use crate::files::{SqlFile, SqlFileSet};

    #[test]
    fn parsed_sql_file_parses_single_statement() -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("parsed_sql_file_single_stmt_test");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let file_path = base.join("one.sql");
        let sql = "CREATE TABLE users (id INTEGER PRIMARY KEY);";
        fs::write(&file_path, sql)?;
        let sql_file = SqlFile::new(&file_path)?;
        let parsed = ParsedSqlFile::parse(sql_file)?;
        assert_eq!(parsed.path(), Some(file_path.as_path()));
        assert_eq!(parsed.content(), sql);
        assert_eq!(parsed.statements().len(), 1);
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }

    #[test]
    fn parsed_sql_file_set_parses_multiple_files() -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("parsed_sql_file_set_multi_test");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let sub = base.join("subdir");
        fs::create_dir_all(&sub)?;
        let file1 = base.join("one.sql");
        let file2 = sub.join("two.sql");
        let sql1 = "CREATE TABLE users (id INTEGER PRIMARY KEY);";
        let sql2 = "CREATE TABLE posts (id INTEGER PRIMARY KEY);";
        fs::write(&file1, sql1)?;
        fs::write(&file2, sql2)?;
        let set = SqlFileSet::new(&base, &[])?;
        let parsed_set = ParsedSqlFileSet::parse_all(set)?;
        let existing_files = parsed_set.files();
        assert_eq!(existing_files.len(), 2);
        for parsed in existing_files {
            assert_eq!(parsed.statements().len(), 1);
            let stmt = &parsed.statements()[0];
            match stmt {
                Statement::CreateTable { .. } => {}
                other => panic!("expected CreateTable, got: {other:?}"),
            }
        }

        let _ = fs::remove_dir_all(&base);
        Ok(())
    }
}
