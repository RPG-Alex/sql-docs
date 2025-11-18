//! main file. for testing. shouldn't be part of crate
use std::fs;
use std::io;
use std::path::Path;

pub mod files;

fn main() -> io::Result<()> {
    let path: &Path = Path::new("/home/alex/Projects/sql-docs/sql_files/");

    let sql_file_set = files::SqlFileSet::new(path)?;
    
    for sql in sql_file_set.iter() {
        println!("{:?}",sql);
    }

    Ok(())
}
