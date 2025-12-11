# Sql File Comment and Statement Parsing for Documentation

[![License](https://img.shields.io/badge/GPL-3%20-blue.svg)](https://opensource.org/license/gpl-3-0)
[![Codecov](https://codecov.io/gh/rpg-alex/sql-docs/branch/main/graph/badge.svg)](https://codecov.io/gh/rpg-alex)
This crate extracts *documentation* from SQL files by parsing:

- SQL statements (via [DataFusion’s SQL Parser](https://github.com/apache/datafusion-sqlparser-rs/))
- Inline and multiline comments
- Table and column locations

It then attaches comments to the SQL structures they describe.

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
use sql_docs::SqlDoc;
use sql_docs::error::DocError;
use std::{env, fs, path::Path};

fn main() -> Result<(), DocError> {
    // tmp dir and file created for demonstration purposes
    let base = env::temp_dir().join("tmp_dir");
    let _ = fs::remove_dir_all(&base);
    fs::create_dir_all(&base)?;
    let example = base.join("example.sql");
    fs::write(&example, r#"/* Table storing user accounts */
    CREATE TABLE users (
        /* Primary key for each user */
        id INTEGER PRIMARY KEY,
        -- The user's login name
        username VARCHAR(255) NOT NULL,
        /* User's email address */
        email VARCHAR(255) UNIQUE NOT NULL
    );"#)?;

    // Extract table & column documentation
    let docs = SqlDoc::from_path(&example).build()?;
    // Verify that the table name is correct
    assert_eq!(docs.tables().first().expect("hardcoded value").name(), "users");
    // Verify extraction of table comment
    assert_eq!(docs.tables().first().expect("hardcoded value").doc(), Some("Table storing user accounts"));
    // Verify First Column name and comment
    assert_eq!(docs.tables().first().expect("hardcoded value").columns().first().expect("hardcoded value").name(), "id");
    assert_eq!(docs.tables().first().expect("hardcoded value").columns().first().expect("hardcoded value").doc(), Some("Primary key for each user"));
    let _ = fs::remove_dir_all(&base);
    Ok(())
}
```
## Use cases

This crate is designed for generating documentation from SQL schemas by attaching comments to:
- Tables
- Columns

using only comments that immediately precede the items they describe.

This makes it ideal for:
- Auto-generating Markdown or HTML documentation
- Building database schema explorers
- Enforcing documentation rules in CI
