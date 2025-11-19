//! main file. for testing. shouldn't be part of crate
use std::error::Error;
use std::path::Path;

pub mod files;

fn main() -> Result<(), Box<dyn Error>> {
    let path: &Path = Path::new("/home/alex/Projects/sql-docs/sql_files/");

    let sql_file_set = files::SqlFileSet::new(path, None)?;
    
    for sql in sql_file_set.iter() {
        println!("{:?}",sql);
    }

    Ok(())
}
