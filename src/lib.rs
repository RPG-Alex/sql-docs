#![doc = include_str!("../README.md")]
//!
//! ## Module layout
//!
//! - [`files`]    — Discover and load `.sql` files from disk
//! - [`ast`]      — Parse SQL into an AST using [`sqlparser`]
//! - [`comments`] — Extract and model SQL comments and spans
//! - [`docs`]     — Generate structured documentation (`TableDoc`, `ColumnDoc`)
//! - [`sql_doc`]  — Build the top-level [`SqlDoc`] and primary entry point
//!
//! **Start here:** [`SqlDoc::from_dir`] or [`SqlDoc::from_path`]

pub mod ast;
pub mod comments;
pub mod docs;
pub mod error;
pub mod files;
pub mod source;
pub mod sql_doc;
pub use sql_doc::SqlDoc;
pub mod dialect;
