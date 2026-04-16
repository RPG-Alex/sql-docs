#![doc = include_str!("../README.md")]
#![deny(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]
#![deny(rustdoc::private_intra_doc_links)]
#![cfg_attr(not(feature = "std"), no_std)]
//!
//! ## Module layout
//!
//! - [`files`]    — Discover and load `.sql` files from disk
//! - [`ast`]      — Parse SQL into an AST using [`sqlparser`]
//! - [`comments`] — Extract and model SQL comments and spans
//! - [`docs`]     — Generate structured documentation (`TableDoc`, `ColumnDoc`)
//! - [`sql_doc`]  — Build the top-level [`SqlDoc`] and primary entry point
//!
//! **Start here:** [`SqlDoc::from_dir`], [`SqlDoc::from_path`], or [`SqlDoc::builder_from_str`]
extern crate alloc;

pub mod ast;
pub mod comments;
pub mod docs;
pub mod error;
#[cfg(feature = "std")]
pub mod files;
pub mod source;
pub mod sql_doc;
pub use crate::{
    comments::{LeadingCommentCapture, MultiFlatten},
    docs::{ColumnDoc, TableDoc},
    error::DocError,
    sql_doc::{SqlDoc, SqlDocBuilder},
};

/// Common imports for typical usage of this crate.
pub mod prelude {
    pub use crate::{
        AnsiDialect, BigQueryDialect, ClickHouseDialect, ColumnDoc, DatabricksDialect, Dialect,
        DocError, DuckDbDialect, GenericDialect, HiveDialect, LeadingCommentCapture, MsSqlDialect,
        MultiFlatten, MySqlDialect, OracleDialect, PostgreSqlDialect, RedshiftSqlDialect,
        SQLiteDialect, SnowflakeDialect, SqlDoc, SqlDocBuilder, TableDoc,
    };
}

pub use sqlparser::dialect::{
    AnsiDialect, BigQueryDialect, ClickHouseDialect, DatabricksDialect, Dialect, DuckDbDialect,
    GenericDialect, HiveDialect, MsSqlDialect, MySqlDialect, OracleDialect, PostgreSqlDialect,
    RedshiftSqlDialect, SQLiteDialect, SnowflakeDialect,
};
