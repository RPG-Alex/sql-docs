#![doc = include_str!("../README.md")]
//! Module layout:
//! - [`files`]    : discover and load `.sql` files from disk
//! - [`ast`]      : parse SQL into an AST using [`sqlparser`]
//! - [`comments`] : extract and model comments and spans
//! - [`docs`]     : Generates a struct to hold [`docs::TableDoc`]
//! - [`sql_doc`]  : Creates the [`SqlDoc`] structure

pub mod ast;
pub mod comments;
pub mod docs;
pub mod error;
pub mod files;
pub mod sql_doc;
pub use sql_doc::SqlDoc;
