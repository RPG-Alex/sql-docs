//! Parse SQL text into an AST (`sqlparser`) for downstream comment attachment.
//!
//! This module does not interpret semantics; it only produces an AST + file metadata.

use alloc::vec::Vec;

use sqlparser::{
    ast::Statement,
    dialect::Dialect,
    parser::{Parser, ParserError},
};

use crate::source::SqlSource;

/// A single SQL Source (such as a file) plus all [`Statement`].
#[derive(Debug)]
pub struct ParsedSqlSource {
    source: SqlSource,
    statements: Vec<Statement>,
}

#[cfg(feature = "std")]
impl ParsedSqlSource {
    /// Getter method for returning the current object' source's path
    #[must_use]
    pub fn path(&self) -> Option<&std::path::Path> {
        self.source.path()
    }

    /// Getter that returns an [`std::path::PathBuf`] for the path rather than `&Path`
    #[must_use]
    pub fn path_into_path_buf(&self) -> Option<std::path::PathBuf> {
        self.source.path_into_path_buf()
    }
}

impl ParsedSqlSource {
    /// Parses a [`SqlSource`] into `sqlparser` [`Statement`] nodes.
    ///
    /// This is the AST layer used by the `comments` module to attach leading
    /// comment spans to statements/columns.
    ///
    /// # Parameters
    /// - `source`: the [`SqlSource`] to parse
    ///
    /// # Errors
    /// - Returns [`ParserError`] if parsing fails
    pub fn parse<D>(source: SqlSource) -> Result<Self, ParserError>
    where
        D: Dialect + Default,
    {
        let statements = Parser::parse_sql(&D::default(), source.content())?;
        Ok(Self { source, statements })
    }

    /// Getter method for returning the [`SqlSource`]
    #[must_use]
    pub const fn source(&self) -> &SqlSource {
        &self.source
    }
    /// Getter for the source's content
    #[must_use]
    pub fn content(&self) -> &str {
        self.source.content()
    }
}

impl ParsedSqlSource {
    /// Getter method for returning the vector of all statements [`Statement`]
    #[must_use]
    pub fn statements(&self) -> &[Statement] {
        &self.statements
    }
}

/// Struct to contain the vector of parsed SQL sources
#[derive(Debug)]
pub struct ParsedSqlSourceSet {
    sources: Vec<ParsedSqlSource>,
}

impl ParsedSqlSourceSet {
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
        let sources =
            set.into_iter().map(ParsedSqlSource::parse::<D>).collect::<Result<Vec<_>, _>>()?;

        Ok(Self { sources })
    }

    /// Getter method for returning the vector of all [`ParsedSqlSource`]
    #[must_use]
    pub fn sources(&self) -> &[ParsedSqlSource] {
        &self.sources
    }
}

#[cfg(test)]
mod tests {
    use alloc::{borrow::ToOwned, boxed::Box};

    use sqlparser::dialect::{GenericDialect, PostgreSqlDialect};

    use super::*;
    use crate::source::SqlSource;

    #[cfg(feature = "std")]
    #[test]
    fn parsed_sql_file_parses_single_statement() -> Result<(), Box<dyn std::error::Error>> {
        let base = std::env::temp_dir().join("parsed_sql_file_single_stmt_test");
        let _ = std::fs::remove_dir_all(&base);
        std::fs::create_dir_all(&base)?;
        let file_path = base.join("one.sql");
        let sql = "CREATE TABLE users (id INTEGER PRIMARY KEY);";
        std::fs::write(&file_path, sql)?;
        let sql_file = SqlSource::from_path(&file_path)?;
        let parsed = ParsedSqlSource::parse::<GenericDialect>(sql_file)?;
        assert_eq!(parsed.path(), Some(file_path.as_path()));
        assert_eq!(parsed.content(), sql);
        assert_eq!(parsed.statements().len(), 1);
        let _ = std::fs::remove_dir_all(&base);
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn parsed_sql_file_set_parses_multiple_files() -> Result<(), Box<dyn std::error::Error>> {
        let base = std::env::temp_dir().join("parsed_sql_file_set_multi_test");
        let _ = std::fs::remove_dir_all(&base);
        std::fs::create_dir_all(&base)?;
        let sub = base.join("subdir");
        std::fs::create_dir_all(&sub)?;
        let file1 = base.join("one.sql");
        let file2 = sub.join("two.sql");
        let sql1 = "CREATE TABLE users (id INTEGER PRIMARY KEY);";
        let sql2 = "CREATE TABLE posts (id INTEGER PRIMARY KEY);";
        std::fs::write(&file1, sql1)?;
        std::fs::write(&file2, sql2)?;
        let set = SqlSource::sql_sources(&base, &[])?;
        let parsed_set = ParsedSqlSourceSet::parse_all::<GenericDialect>(set)?;
        let existing_files = parsed_set.sources();
        assert_eq!(existing_files.len(), 2);
        for parsed in existing_files {
            assert_eq!(parsed.statements().len(), 1);
            let stmt = &parsed.statements()[0];
            match stmt {
                Statement::CreateTable { .. } => {}
                other => panic!("expected CreateTable, got: {other:?}"),
            }
        }

        let _ = std::fs::remove_dir_all(&base);
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn parsed_sql_file_path_into_path_buf_round_trips() -> Result<(), Box<dyn std::error::Error>> {
        let base = std::env::temp_dir().join("parsed_sql_file_path_into_path_buf_round_trips");
        let _ = std::fs::remove_dir_all(&base);
        std::fs::create_dir_all(&base)?;
        let file_path = base.join("one.sql");
        let sql = "CREATE TABLE t (id INTEGER PRIMARY KEY);";
        std::fs::write(&file_path, sql)?;
        let sql_file = SqlSource::from_path(&file_path)?;
        let parsed = ParsedSqlSource::parse::<GenericDialect>(sql_file)?;
        assert_eq!(parsed.path_into_path_buf(), Some(file_path));
        let _ = std::fs::remove_dir_all(&base);
        Ok(())
    }

    #[test]
    fn parsed_sql_file_parse_postgres_handles_pg_function_syntax()
    -> Result<(), Box<dyn core::error::Error>> {
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

        let src = SqlSource::from(sql.to_owned());
        let parsed = ParsedSqlSource::parse::<PostgreSqlDialect>(src)?;
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

    #[cfg(feature = "std")]
    #[test]
    fn parsed_sql_file_set_parse_all_uses_default_dialect() -> Result<(), Box<dyn std::error::Error>>
    {
        let base = std::env::temp_dir().join("parsed_sql_file_set_parse_all_default_dialect");
        let _ = std::fs::remove_dir_all(&base);
        std::fs::create_dir_all(&base)?;

        let file1 = base.join("one.sql");
        let file2 = base.join("two.sql");

        std::fs::write(&file1, "CREATE TABLE t1 (id INTEGER PRIMARY KEY);")?;

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
        std::fs::write(&file2, pg_sql)?;

        let set = SqlSource::sql_sources(&base, &[])?;
        let parsed_set = ParsedSqlSourceSet::parse_all::<GenericDialect>(set)?;

        assert_eq!(parsed_set.sources().len(), 2);

        for parsed in parsed_set.sources() {
            assert!(
                parsed.statements().iter().any(|s| matches!(s, Statement::CreateTable { .. })),
                "expected CreateTable in parsed file; got statements: {:?}",
                parsed.statements()
            );
        }

        let _ = std::fs::remove_dir_all(&base);
        Ok(())
    }

    #[test]
    fn parsed_sql_file_parse_invalid_sql_returns_error() {
        let sql = "CREATE TABLE";
        let src = SqlSource::from(sql.to_owned());
        let res = ParsedSqlSource::parse::<GenericDialect>(src);
        assert!(res.is_err(), "expected parse to fail for invalid SQL");
    }
}
