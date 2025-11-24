//! Module layout:
//! - [`files`]    : discover and load `.sql` files from disk
//! - [`ast`]      : parse SQL into an AST using `sqlparser`
//! - [`comments`] : extract and model comments + spans
pub mod files;
pub mod ast;
pub mod comments;
pub mod docs;