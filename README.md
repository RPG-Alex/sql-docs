# Sql File Comment and Statement Parsing for Documentation

[![License](https://img.shields.io/badge/GPL-3%20-blue.svg)](https://opensource.org/license/gpl-3-0)

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
```rust,no_run
use sql_docs::generate_docs_from_dir_no_deny;
use std::path::Path;

fn main() -> Result<(), sql_docs::DocError> {
    // Directory containing your .sql files
    let directory = "sql_dir";

    // Extract table + column documentation
    let docs = generate_docs_from_dir_no_deny(directory)?;

    // Format and print each table and table's columns
    for (path, sql_docs) in docs {
        println!("File: {}", path.display());
        for table in sql_docs.tables() {
            println!("  Table: {}", table.name());
            if let Some(doc) = table.doc() {
                println!("    Comment: {}", doc);
            }
            println!("    Columns:");
            for column in table.columns() {
                println!("      - {}", column.name());
                if let Some(col_doc) = column.doc() {
                    println!("          {}", col_doc);
                }
            }
        }
    }

    Ok(())
}
```
Which should output:
```bash
File: sql_dir/users.sql
  Table: users
    Comment: Table storing user accounts
    Columns:
      - id
          Primary key for each user
      - username
          The user's login name
      - email
          User's email address
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
