//! Module for handling dialect selection for parsing
use sqlparser::dialect::AnsiDialect;
use sqlparser::dialect::BigQueryDialect;
use sqlparser::dialect::ClickHouseDialect;
use sqlparser::dialect::DatabricksDialect;
use sqlparser::dialect::DuckDbDialect;
use sqlparser::dialect::GenericDialect;
use sqlparser::dialect::HiveDialect;
use sqlparser::dialect::MsSqlDialect;
use sqlparser::dialect::MySqlDialect;
use sqlparser::dialect::OracleDialect;
use sqlparser::dialect::PostgreSqlDialect;
use sqlparser::dialect::RedshiftSqlDialect;
use sqlparser::dialect::SQLiteDialect;
use sqlparser::dialect::SnowflakeDialect;

/// Enum used for SQL dialect selection
#[derive(PartialEq, Eq)]
pub enum Dialect {
    /// ANSI SQL dialect
    Ansi,
    /// Google BigQuery SQL dialect
    BigQuery,
    /// ClickHouse SQL dialect
    ClickHouse,
    /// Databricks SQL dialect
    Databricks,
    /// DuckDB SQL dialect
    DuckDb,
    /// Generic SQL dialect
    Generic,
    /// Apache Hive SQL dialect
    Hive,
    /// Microsoft SQL Server (T-SQL) dialect
    MsSql,
    /// MySQL SQL dialect
    MySql,
    /// Oracle SQL dialect
    Oracle,
    /// PostgreSQL SQL dialect
    PostgreSql,
    /// Amazon Redshift SQL dialect
    RedshiftSql,
    /// SQLite SQL dialect
    SQLite,
    /// Snowflake SQL dialect
    Snowflake,
}

use sqlparser::dialect::Dialect as SqlDialectTrait;

impl Dialect {
    /// Execute a function with the selected `sqlparser` dialect.
    pub fn with_sqlparser_dialect<R>(
        self,
        f: impl FnOnce(&dyn SqlDialectTrait) -> R,
    ) -> R {
        match self {
            Dialect::Ansi => f(&AnsiDialect {}),
            Dialect::BigQuery => f(&BigQueryDialect {}),
            Dialect::ClickHouse => f(&ClickHouseDialect {}),
            Dialect::Databricks => f(&DatabricksDialect {}),
            Dialect::DuckDb => f(&DuckDbDialect {}),
            Dialect::Generic => f(&GenericDialect {}),
            Dialect::Hive => f(&HiveDialect {}),
            Dialect::MsSql => f(&MsSqlDialect {}),
            Dialect::MySql => f(&MySqlDialect {}),
            Dialect::Oracle => f(&OracleDialect {}),
            Dialect::PostgreSql => f(&PostgreSqlDialect {}),
            Dialect::RedshiftSql => f(&RedshiftSqlDialect {}),
            Dialect::SQLite => f(&SQLiteDialect {}),
            Dialect::Snowflake => f(&SnowflakeDialect {}),
        }
    }
}