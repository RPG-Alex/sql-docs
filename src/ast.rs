//! Parse SQL text into an AST (`sqlparser`) for downstream comment attachment.
//!
//! This module does not interpret semantics; it only produces an AST + file metadata.

use sqlparser::{
    ast::Statement,
    dialect::Dialect,
    parser::{Parser, ParserError},
};

use alloc::vec::Vec;

use crate::source::SqlSource;

/// A single SQL file plus all [`Statement`].
#[derive(Debug)]
pub struct ParsedSqlFile {
    file: SqlSource,
    statements: Vec<Statement>,
}

impl ParsedSqlFile {
    /// Parses a [`SqlSource`] into `sqlparser` [`Statement`] nodes.
    ///
    /// This is the AST layer used by the `comments` module to attach leading
    /// comment spans to statements/columns.
    ///
    /// # Parameters
    /// - `file`: the [`SqlSource`] to parse
    ///
    /// # Errors
    /// - Returns [`ParserError`] if parsing fails
    pub fn parse<D>(file: SqlSource) -> Result<Self, ParserError>
    where
        D: Dialect + Default,
    {
        let statements = Parser::parse_sql(&D::default(), file.content())?;
        Ok(Self { file, statements })
    }

    /// Getter method for returning the [`SqlSource`]
    #[must_use]
    pub const fn file(&self) -> &SqlSource {
        &self.file
    }

    /// Getter method for returning the current object's file's path
    #[must_use]
    pub fn path(&self) -> Option<&std::path::Path> {
        self.file.path()
    }

    /// Getter that returns an [`std::path::PathBuf`] for the path rather than `&Path`
    #[must_use]
    pub fn path_into_path_buf(&self) -> Option<std::path::PathBuf> {
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
    /// Method that parses a set of all members in a [`SqlSource`]
    ///
    /// # Parameters
    /// - `set` the set of [`SqlSource`]
    ///
    /// # Errors
    /// - [`ParserError`] is returned for any errors parsing
    pub fn parse_all<D>(set: Vec<SqlSource>) -> Result<Self, ParserError>
    where
        D: Dialect + Default,
    {
        let files =
            set.into_iter().map(ParsedSqlFile::parse::<D>).collect::<Result<Vec<_>, _>>()?;

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
    use alloc::borrow::ToOwned;
    use alloc::boxed::Box;
    use sqlparser::dialect::{GenericDialect, PostgreSqlDialect};
    use std::{env, fs};

    use super::*;
    use crate::source::SqlSource;

    #[test]
    fn parsed_sql_file_parses_single_statement() -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("parsed_sql_file_single_stmt_test");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let file_path = base.join("one.sql");
        let sql = "CREATE TABLE users (id INTEGER PRIMARY KEY);";
        fs::write(&file_path, sql)?;
        let sql_file = SqlSource::from_path(&file_path)?;
        let parsed = ParsedSqlFile::parse::<GenericDialect>(sql_file)?;
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
        let set = SqlSource::sql_sources(&base, &[])?;
        let parsed_set = ParsedSqlFileSet::parse_all::<GenericDialect>(set)?;
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

    #[test]
    fn parsed_sql_file_path_into_path_buf_round_trips() -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("parsed_sql_file_path_into_path_buf_round_trips");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let file_path = base.join("one.sql");
        let sql = "CREATE TABLE t (id INTEGER PRIMARY KEY);";
        fs::write(&file_path, sql)?;
        let sql_file = SqlSource::from_path(&file_path)?;
        let parsed = ParsedSqlFile::parse::<GenericDialect>(sql_file)?;
        assert_eq!(parsed.path_into_path_buf(), Some(file_path));
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }

    #[test]
    fn parsed_sql_file_parse_postgres_handles_pg_function_syntax()
    -> Result<(), Box<dyn std::error::Error>> {
        let sql = r"
            CREATE OR REPLACE FUNCTION f()
            RETURNS SMALLINT
            LANGUAGE plpgsql
            SECURITY DEFINER
            STABLE
            AS $$
            BEGIN
                RETURN 4;
            END;
            $$;

            CREATE TABLE t (id INTEGER PRIMARY KEY);
        ";

        let src = SqlSource::from_str(sql.to_owned(), None);
        let parsed = ParsedSqlFile::parse::<PostgreSqlDialect>(src)?;
        assert!(
            parsed.statements().len() >= 2,
            "expected at least 2 statements (function + table)"
        );
        assert!(
            parsed.statements().iter().any(|s| matches!(s, Statement::CreateTable { .. })),
            "expected at least one CreateTable statement"
        );
        Ok(())
    }

    #[test]
    fn parsed_sql_file_set_parse_all_uses_default_dialect() -> Result<(), Box<dyn std::error::Error>>
    {
        let base = env::temp_dir().join("parsed_sql_file_set_parse_all_default_dialect");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;

        let file1 = base.join("one.sql");
        let file2 = base.join("two.sql");

        fs::write(&file1, "CREATE TABLE t1 (id INTEGER PRIMARY KEY);")?;

        let pg_sql = r"
            CREATE OR REPLACE FUNCTION f()
            RETURNS SMALLINT
            LANGUAGE plpgsql
            SECURITY DEFINER
            STABLE
            AS $$
            BEGIN
                RETURN 4;
            END;
            $$;

            CREATE TABLE t2 (id INTEGER PRIMARY KEY);
        ";
        fs::write(&file2, pg_sql)?;

        let set = SqlSource::sql_sources(&base, &[])?;
        let parsed_set = ParsedSqlFileSet::parse_all::<GenericDialect>(set)?;

        assert_eq!(parsed_set.files().len(), 2);

        for parsed in parsed_set.files() {
            assert!(
                parsed.statements().iter().any(|s| matches!(s, Statement::CreateTable { .. })),
                "expected CreateTable in parsed file; got statements: {:?}",
                parsed.statements()
            );
        }

        let _ = fs::remove_dir_all(&base);
        Ok(())
    }

    #[test]
    fn parsed_sql_file_parse_invalid_sql_returns_error() {
        let sql = "CREATE TABLE";
        let src = SqlSource::from_str(sql.to_owned(), None);
        let res = ParsedSqlFile::parse::<GenericDialect>(src);
        assert!(res.is_err(), "expected parse to fail for invalid SQL");
    }
}
