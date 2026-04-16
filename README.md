# Sql File Comment and Statement Parsing for Documentation

[![License](https://img.shields.io/badge/GPL-3%20-blue.svg)](https://opensource.org/license/gpl-3-0)
[![Codecov](https://codecov.io/gh/rpg-alex/sql-docs/branch/main/graph/badge.svg)](https://codecov.io/gh/rpg-alex)
[![CI](https://github.com/RPG-Alex/sql-docs/actions/workflows/rust.yml/badge.svg)](https://github.com/RPG-Alex/sql-docs/actions/workflows/rust.yml)
[![Documentation](https://docs.rs/sql_docs/badge.svg)](https://docs.rs/sql_docs)
 
This crate extracts **documentation** from SQL files by parsing:

- SQL statements (via [`sqlparser`](https://github.com/sqlparser-rs/sqlparser-rs))
- Line and block comments
- Table and column locations

Then comments are attached to the SQL structures they describe, producing a structured, queryable documentation model.

## Example 

With a directory structured like this:
```bash
sql_dir/
└── users.sql
```
and the content of `users.sql` being:
```sql
/* Table storing user accounts */
CREATE TABLE users (
    /* Primary key for each user */
    id INTEGER PRIMARY KEY,
    -- The user's login name
    username VARCHAR(255) NOT NULL,
    /* User's email address */
    email VARCHAR(255) UNIQUE NOT NULL
);
```
A rudimentary implementation can be generated with:
```rust
#![cfg(feature = "std")]
use sql_docs::{GenericDialect,SqlDoc,error::DocError};
use std::{env, fs};

fn main() -> Result<(), DocError> {
    // Temporary directory and file created for demonstration purposes
    let base = env::temp_dir().join("tmp_dir");
    let _ = fs::remove_dir_all(&base);
    fs::create_dir_all(&base)?;
    let example = base.join("example.sql");

    fs::write(
        &example,
        r#"-- Table storing user accounts
-- Contains all user values
/* Rows generated at registration */
CREATE TABLE users (
    /* Primary key for each user */
    id INTEGER PRIMARY KEY,
    -- The user's login name
    username VARCHAR(255) NOT NULL,
    /* User's email address */
    email VARCHAR(255) UNIQUE NOT NULL
);"#,
    )?;

    // Extract documentation from a single file
    let docs = SqlDoc::from_path(&example)
        // Capture all valid comment lines preceding the statements directly
        .collect_all_leading()
        // Replace `\n` with a `str`
        .flatten_multiline_with(". ")
        // Finally build the `SqlDoc`
        .build::<GenericDialect>()?;
    // Or extract recursively from a directory
    // let docs = SqlDoc::from_dir(&base).build()?;

    // Retrieve a specific table
    let users = docs.table("users", None)?;

    // Table name
    assert_eq!(users.name(), "users");
    // Optional table-level documentation
    assert_eq!(users.doc(), Some("Table storing user accounts. Contains all user values. Rows generated at registration"));
    // Path to the source file
    assert_eq!(users.path(), Some(example.as_ref()));

    let _ = fs::remove_dir_all(&base);
    Ok(())
}
```

Or with no `std` and from a `String`:
```rust
use sql_docs::{GenericDialect,SqlDoc,error::DocError};

fn main() -> Result<(), DocError> {
    // Temporary directory and file created for demonstration purposes
    let example = 
        r#"-- Table storing user accounts
-- Contains all user values
/* Rows generated at registration */
CREATE TABLE users (
    /* Primary key for each user */
    id INTEGER PRIMARY KEY,
    -- The user's login name
    username VARCHAR(255) NOT NULL,
    /* User's email address */
    email VARCHAR(255) UNIQUE NOT NULL
);"#;

    // Extract documentation from a single file
    let docs = SqlDoc::from(&example)
        // Capture all valid comment lines preceding the statements directly
        .collect_all_leading()
        // Replace `\n` with a `str`
        .flatten_multiline_with(". ")
        // Finally build the `SqlDoc`
        .build::<GenericDialect>()?;
    // Or extract recursively from a directory
    // let docs = SqlDoc::from_dir(&base).build()?;

    // Retrieve a specific table
    let users = docs.table("users", None)?;

    // Table name
    assert_eq!(users.name(), "users");
    // Optional table-level documentation
    assert_eq!(users.doc(), Some("Table storing user accounts. Contains all user values. Rows generated at registration"));
    Ok(())
}
```
## Primary Interface (Main API)

These are the primary entry points most users will interact with:

* [`SqlDoc::from_path`](https://docs.rs/sql-docs/latest/sql_docs/sql_doc/struct.SqlDoc.html#method.from_path) Build documentation from a single `.sql` file.
* [`SqlDoc::from_dir`](https://docs.rs/sql-docs/latest/sql_docs/sql_doc/struct.SqlDoc.html#method.from_dir) Recursively scan a directory for `.sql` files and build documentation.
* [`SqlDocBuilder::build`](https://docs.rs/sql-docs/latest/sql_docs/sql_doc/struct.SqlDocBuilder.html#method.build) Finalize the builder and produce a [`SqlDoc`].
* [`SqlDocBuilder::deny`](https://docs.rs/sql-docs/latest/sql_docs/sql_doc/struct.SqlDocBuilder.html#method.deny) Exclude specific files by full path.
* [`SqlDocBuilder::flatten_multiline`](https://docs.rs/sql-docs/latest/sql_docs/sql_doc/struct.SqlDocBuilder.html#method.flatten_multiline) Flatten multiline comments into a single line.
* [`SqlDocBuilder::collect_single_nearest`](https://docs.rs/sql-docs/latest/sql_docs/sql_doc/struct.SqlDocBuilder.html#method.collect_single_nearest) Collect only the nearest leading comment.

## Use Cases

This crate is designed for generating documentation from SQL schemas by attaching comments to:

* Tables
* Columns

using **only comments that immediately precede** the items they describe, with the ability to differentiate between multiple leading comments per statement or collect only the nearest comment preceding a statement.

This makes it well-suited for:

* Auto-generating Markdown or HTML documentation
* Building database schema explorers
* Enforcing documentation rules in CI
* Static analysis of SQL schemas

## Design Notes

* Inline and interstitial comments are intentionally ignored.
* Comment attachment is line-based and deterministic.
* One SQL file may define multiple tables.
* No database connection is required.
* `sql_doc` items are sorted by `table` name and `column` name, supporting binary searching