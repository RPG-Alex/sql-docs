//! Module for the top level `SqlDoc` structure.

use std::path::{Path, PathBuf};

use crate::{
    ast::ParsedSqlFile,
    comments::Comments,
    docs::{SqlFileDoc, TableDoc},
    error::DocError,
    files::{SqlFile, SqlFilesList},
};

/// Structure for Sql Documentation, built from [`TableDoc`] and
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SqlDoc {
    /// Holds the [`Vec`] of all tables found in all specified files.
    tables: Vec<TableDoc>,
}

/// Builder structure for the [`SqlDoc`]
#[derive(Debug, Eq, PartialEq)]
pub struct SqlDocBuilder<'a> {
    /// The source for implementing the [`SqlDoc`] to be built
    source: SqlFileDocSource<'a>,
    /// The list of Paths to be ignored for parsing purposes.
    deny: Vec<String>,
    /// Used to indicate maintaining the `[(PathBuf, SqlFileDoc)]`
    retain_files: bool,
    /// Struct for tracking the settings for flattening multiline comments
    multiline_flat: MultiFlatten,
}

/// Enum for multiline comment flattening.
#[derive(Debug, Eq, PartialEq)]
pub enum MultiFlatten {
    /// Default option, retains multiline structure with `\n`
    NoFlat,
    /// Sets multiline comments to be flattened and combined without adding formatting
    FlattenWithNone,
    /// Will flatten comments and amend the content of [`String`] to the end of the former leading lines
    Flatten(String),
}

/// Enum for specifying a file doc source as a `directory` or a specific `file`
#[derive(Debug, Eq, PartialEq)]
enum SqlFileDocSource<'a> {
    Dir(PathBuf),
    File(PathBuf),
    FromString(&'a str),
}

impl SqlDoc {
    /// Method for creating a new [`SqlDoc`]
    #[must_use]
    pub const fn new(tables: Vec<TableDoc>) -> Self {
        Self { tables }
    }
    /// Method for generating builder from a directory.
    pub fn from_dir<P: AsRef<Path> + ?Sized>(root: &P) -> SqlDocBuilder<'_> {
        SqlDocBuilder {
            source: SqlFileDocSource::Dir(root.as_ref().to_path_buf()),
            deny: Vec::new(),
            retain_files: false,
            multiline_flat: MultiFlatten::NoFlat,
        }
    }
    /// Method for generating builder from a [`Path`] of a single file
    pub fn from_path<P: AsRef<Path> + ?Sized>(path: &P) -> SqlDocBuilder<'_> {
        SqlDocBuilder {
            source: SqlFileDocSource::File(path.as_ref().to_path_buf()),
            deny: Vec::new(),
            retain_files: false,
            multiline_flat: MultiFlatten::NoFlat,
        }
    }

    /// Method for finding a specific [`TableDoc`] by `name`
    ///
    /// # Parameters
    /// - the table `name` as a [`str`]
    ///
    /// # Errors
    /// - Will return [`DocError::TableNotFound`] if the expected table is not found
    /// - Will return [`DocError::DuplicateTablesFound`] if more than one table is found
    pub fn table(&self, table: &str) -> Result<&TableDoc, DocError> {
        let matches = self
            .tables
            .iter()
            .filter(|table_doc| table_doc.name() == table)
            .collect::<Vec<&TableDoc>>();
        match matches.as_slice() {
            [] => Err(DocError::TableNotFound { name: table.to_string() }),
            [only] => Ok(*only),
            _ => Err(DocError::DuplicateTablesFound {
                tables: matches.into_iter().cloned().collect(),
            }),
        }
    }

    /// Method for finding a specific [`TableDoc`] from `schema` and table `name`
    ///
    /// # Parameters
    /// - the table's `schema` as a [`str`]
    /// - the table's `name` as a [`str`]
    ///
    /// # Errors
    /// - Will return [`DocError::TableNotFound`] if the expected table is not found
    /// - Will return [`DocError::DuplicateTablesFound`] if more than one table is found
    pub fn table_with_schema(&self, schema: &str, name: &str) -> Result<&TableDoc, DocError> {
        let matches = self
            .tables
            .iter()
            .filter(|table_doc| table_doc.name() == name && table_doc.schema() == Some(schema))
            .collect::<Vec<&TableDoc>>();
        match matches.as_slice() {
            [] => Err(DocError::TableNotFound { name: name.to_string() }),
            [only] => Ok(*only),
            _ => Err(DocError::DuplicateTablesFound {
                tables: matches.into_iter().cloned().collect(),
            }),
        }
    }

    /// Getter method for returning the `&[TableDoc]`
    #[must_use]
    pub fn tables(&self) -> &[TableDoc] {
        &self.tables
    }
    /// Getter that returns a mutable reference to the [`TableDoc`]
    #[must_use]
    pub fn tables_mut(&mut self) -> &mut [TableDoc] {
        &mut self.tables
    }
    /// Method to move tables out of Structure if needed
    #[must_use]
    pub fn into_tables(self) -> Vec<TableDoc> {
        self.tables
    }
}

impl SqlDocBuilder<'_> {
    /// Method for adding an item to the deny list
    ///
    /// # Parameters
    /// - The `path` that will be added to deny path `Vec`
    #[must_use]
    pub fn deny(mut self, deny_path: &str) -> Self {
        self.deny.push(deny_path.into());
        self
    }

    /// Setter that ticks on the option to retain the [`Vec<(PathBuf,SqlFileDoc)>`]
    #[must_use]
    pub const fn retain_files(mut self) -> Self {
        self.retain_files = true;
        self
    }

    /// Method for flattening the multiline comments without additional formatting
    #[must_use]
    pub fn flatten_multiline(mut self) -> Self {
        self.multiline_flat = MultiFlatten::FlattenWithNone;
        self
    }

    /// Method for flattening the multiline comments with [`String`] containing additional leading line formatting to add, such as punctuation.
    #[must_use]
    pub fn flatten_multiline_with(mut self, suffix: &str) -> Self {
        self.multiline_flat = MultiFlatten::Flatten(suffix.to_string());
        self
    }
    /// Method to set multiline comments to preserver multiple lines
    #[must_use]
    pub fn preserve_multiline(mut self) -> Self {
        self.multiline_flat = MultiFlatten::NoFlat;
        self
    }
    /// Builds the [`SqlDoc`]
    ///
    /// # Errors
    /// - Will return `DocError` bubbled up
    pub fn build(self) -> Result<SqlDoc, DocError> {
        let docs: Vec<SqlFileDoc> = match &self.source {
            SqlFileDocSource::Dir(path) => generate_docs_from_dir(path, &self.deny)?,
            SqlFileDocSource::File(file) => {
                let gen_files = generate_docs_from_file(file)?;
                let sql_doc = gen_files;
                vec![sql_doc]
            }
            SqlFileDocSource::FromString(content) => {
                todo!()
            }
        };
        let num_of_tables = docs.iter().map(super::docs::SqlFileDoc::number_of_tables).sum();
        let mut tables = Vec::with_capacity(num_of_tables);
        if self.retain_files {
            let files = docs;
            for sql_doc in &files {
                tables.extend(sql_doc.clone());
            }
        } else {
            for sql_doc in docs {
                tables.extend(sql_doc);
            }
        }
        let mut sql_doc = SqlDoc { tables };
        match self.multiline_flat {
            MultiFlatten::FlattenWithNone => {
                sql_doc = flatten_docs(sql_doc, None);
            }
            MultiFlatten::Flatten(flat_format) => {
                sql_doc = flatten_docs(sql_doc, Some(&flat_format));
            }
            MultiFlatten::NoFlat => {}
        }
        Ok(sql_doc)
    }
}

fn flatten_docs(mut sql_doc: SqlDoc, flat_format: Option<&str>) -> SqlDoc {
    let format = flat_format.map_or("", |c| c);
    for table in sql_doc.tables_mut() {
        if let Some(doc) = table.doc() {
            let new_doc = flatten_lines(doc.lines(), format);
            table.set_doc(new_doc);
        }
        for column in table.columns_mut() {
            if let Some(doc) = column.doc() {
                let new_doc = flatten_lines(doc.lines(), format);
                column.set_doc(new_doc);
            }
        }
    }

    sql_doc
}

fn flatten_lines<'a>(lines: impl Iterator<Item = &'a str>, sep: &str) -> String {
    let mut out = String::new();
    for (i, line) in lines.enumerate() {
        if i > 0 {
            out.push_str(sep);
        }
        out.push_str(line);
    }
    out
}

fn generate_docs_from_dir<P: AsRef<Path>, S: AsRef<str>>(
    source: P,
    deny: &[S],
) -> Result<Vec<SqlFileDoc>, DocError> {
    let deny_list: Vec<String> = deny.iter().map(|file| file.as_ref().to_string()).collect();
    let deny_option = if deny_list.is_empty() { None } else { Some(deny_list) };
    let file_set = SqlFilesList::new(source.as_ref(), deny_option)?;
    let mut sql_docs = Vec::new();
    for file in file_set.sql_files_sorted() {
        let docs = generate_docs_from_file(file)?;
        sql_docs.push(docs);
    }
    Ok(sql_docs)
}

fn generate_docs_from_file<P: AsRef<Path>>(source: P) -> Result<SqlFileDoc, DocError> {
    let file = SqlFile::new(source.as_ref())?;
    let parsed_file = ParsedSqlFile::parse(file)?;
    let comments = Comments::parse_all_comments_from_file(&parsed_file)?;
    let docs = SqlFileDoc::from_parsed_file(&parsed_file, &comments)?;
    Ok(docs)
}

#[cfg(test)]
mod tests {
    use std::{env, fs, path::PathBuf, vec};

    use crate::{
        SqlDoc,
        docs::{ColumnDoc, TableDoc},
        error::DocError,
        sql_doc::{MultiFlatten, SqlDocBuilder},
    };

    #[test]
    fn build_sql_doc_from_file() -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("build_sql_doc_from_file");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let file = base.join("test_file.sql");
        let sample = sample_sql();
        let (contents, expected): (Vec<_>, Vec<_>) = sample.into_iter().unzip();
        fs::write(&file, contents.join(""))?;
        let sql_doc = SqlDoc::from_path(&file).build()?;
        let doc = SqlDoc::new(expected.into_iter().flat_map(SqlDoc::into_tables).collect());
        assert_eq!(sql_doc, doc);
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }
    #[test]
    fn build_sql_doc_from_dir() -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("build_sql_doc_from_dir");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let sample = sample_sql();
        let (contents, expected): (Vec<_>, Vec<_>) = sample.into_iter().unzip();
        for (idx, contents) in contents.iter().enumerate() {
            let path = base.join(format!("test_file{idx}.sql"));
            fs::write(&path, contents)?;
        }
        let sql_doc = SqlDoc::from_dir(&base).build()?;
        let mut actual: Vec<TableDoc> = sql_doc.into_tables();
        dbg!(&actual);
        let mut expected: Vec<TableDoc> =
            expected.into_iter().flat_map(SqlDoc::into_tables).collect();
        assert_eq!(actual.len(), expected.len());
        sort_tables(&mut actual);
        sort_tables(&mut expected);
        assert_eq!(actual, expected);
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }

    #[test]
    fn test_retrieve_table_and_schema() -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("build_sql_doc_with_schema");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let file = base.join("test_file.sql");
        let sample = sample_sql();
        let (contents, expected): (Vec<_>, Vec<_>) = sample.into_iter().unzip();
        fs::write(&file, contents.join(""))?;
        let sql_doc = SqlDoc::from_path(&file).build()?;
        let doc = SqlDoc::new(expected.into_iter().flat_map(SqlDoc::into_tables).collect());
        assert_eq!(sql_doc, doc);
        let table = "users";
        let actual_table = sql_doc.table(table)?;
        let expected_table = doc.table(table)?;
        assert_eq!(actual_table, expected_table);
        let schema = "analytics";
        let schema_table = "events";
        let actual_schema = sql_doc.table_with_schema(schema, schema_table)?;
        let expected_schema = doc.table_with_schema(schema, schema_table)?;
        assert_eq!(actual_schema, expected_schema);
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }

    #[test]
    fn test_table_err() {
        let empty_set = SqlDoc::new(vec![]);
        let empty_table_err = empty_set.table("name");
        assert!(empty_table_err.is_err());
        assert!(matches!(
            empty_table_err,
            Err(DocError::TableNotFound { name }) if name == "name"
        ));
        let duplicate_set = SqlDoc::new(vec![
            TableDoc::new(None, "duplicate".to_string(), None, vec![], None),
            TableDoc::new(None, "duplicate".to_string(), None, vec![], None),
        ]);
        let duplicate_tables_err = duplicate_set.table("duplicate");
        assert!(matches!(duplicate_tables_err, Err(DocError::DuplicateTablesFound { .. })));
    }

    #[test]
    fn test_schema_err() {
        let empty_set = SqlDoc::new(vec![]);
        let empty_table_err = empty_set.table_with_schema("schema", "name");
        assert!(empty_table_err.is_err());
        assert!(matches!(
            empty_table_err,
            Err(DocError::TableNotFound { name }) if name == "name"
        ));
        let duplicate_set = SqlDoc::new(vec![
            TableDoc::new(Some("schema".to_string()), "duplicate".to_string(), None, vec![], None),
            TableDoc::new(Some("schema".to_string()), "duplicate".to_string(), None, vec![], None),
        ]);
        let duplicate_tables_err = duplicate_set.table_with_schema("schema", "duplicate");
        assert!(matches!(duplicate_tables_err, Err(DocError::DuplicateTablesFound { .. })));
    }
    fn sort_tables(tables: &mut [TableDoc]) {
        tables.sort_by(|a, b| {
            let a_key = (a.schema().unwrap_or(""), a.name());
            let b_key = (b.schema().unwrap_or(""), b.name());
            a_key.cmp(&b_key)
        });
    }

    fn sample_sql() -> Vec<(&'static str, SqlDoc)> {
        vec![
            (
                r"
            -- Users table
            CREATE TABLE users (
                -- id
                id INTEGER PRIMARY KEY,
                -- login name
                username TEXT NOT NULL
            );
            ",
                SqlDoc::new(vec![TableDoc::new(
                    None,
                    "users".to_string(),
                    Some("Users table".to_string()),
                    vec![
                        ColumnDoc::new("id".to_string(), Some("id".to_string())),
                        ColumnDoc::new("username".to_string(), Some("login name".to_string())),
                    ],
                    None,
                )]),
            ),
            (
                r"
            /* Posts table */
            CREATE TABLE posts (
                /* primary key */
                id INTEGER PRIMARY KEY,
                title TEXT NOT NULL
            );
            ",
                SqlDoc::new(vec![TableDoc::new(
                    None,
                    "posts".to_string(),
                    Some("Posts table".to_string()),
                    vec![
                        ColumnDoc::new("id".to_string(), Some("primary key".to_string())),
                        ColumnDoc::new("title".to_string(), None),
                    ],
                    None,
                )]),
            ),
            (
                r"
            CREATE TABLE things (
                id INTEGER PRIMARY KEY,
                name TEXT,
                value INTEGER
            );
            ",
                SqlDoc::new(vec![TableDoc::new(
                    None,
                    "things".to_string(),
                    None,
                    vec![
                        ColumnDoc::new("id".to_string(), None),
                        ColumnDoc::new("name".to_string(), None),
                        ColumnDoc::new("value".to_string(), None),
                    ],
                    None,
                )]),
            ),
            (
                r"
            -- Table with schema
            CREATE TABLE analytics.events (
                /* event id */
                id INTEGER PRIMARY KEY,
                /* event payload */
                payload TEXT
            );
            ",
                SqlDoc::new(vec![TableDoc::new(
                    Some("analytics".to_string()),
                    "events".to_string(),
                    Some("Table with schema".to_string()),
                    vec![
                        ColumnDoc::new("id".to_string(), Some("event id".to_string())),
                        ColumnDoc::new("payload".to_string(), Some("event payload".to_string())),
                    ],
                    None,
                )]),
            ),
        ]
    }

    #[test]
    fn test_sql_doc_getters() {
        let tables = vec![TableDoc::new(None, "name".to_string(), None, vec![], None)];
        let sql_doc =
            SqlDoc::new(vec![TableDoc::new(None, "name".to_string(), None, vec![], None)]);
        assert_eq!(sql_doc.tables().len(), 1);
        assert_eq!(sql_doc.tables(), tables);
    }

    #[test]
    fn test_sql_builder_deny_from_path() {
        let actual_builder = SqlDoc::from_path("path").deny("path1").deny("path2").retain_files();
        let expected_builder = SqlDocBuilder {
            source: crate::sql_doc::SqlFileDocSource::File(PathBuf::from("path")),
            deny: vec!["path1".to_string(), "path2".to_string()],
            retain_files: true,
            multiline_flat: MultiFlatten::NoFlat,
        };
        assert_eq!(actual_builder, expected_builder);
    }
    #[test]
    fn test_sql_builder_to_sql_doc() -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("sql_builder_to_sql_doc");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let file = base.join("test_file.sql");
        let sample = sample_sql();
        let (contents, expected): (Vec<_>, Vec<_>) = sample.into_iter().unzip();
        fs::write(&file, contents.join(""))?;
        let sql_doc = SqlDoc::from_path(&file).retain_files().build()?;
        let sql_doc_deny = SqlDoc::from_dir(&base)
            .deny(file.to_str().unwrap_or_else(|| panic!("unable to find file val")))
            .retain_files()
            .build()?;
        let doc_deny = SqlDoc::new(vec![]);
        let doc = SqlDoc::new(expected.into_iter().flat_map(SqlDoc::into_tables).collect());
        assert_eq!(sql_doc, doc);
        assert_eq!(sql_doc_deny, doc_deny);
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }

    #[test]
    fn test_builder_multiflatten_variants() {
        let b1 = SqlDoc::from_path("dummy.sql");
        let b2 = SqlDoc::from_path("dummy.sql").flatten_multiline();
        let b3 = SqlDoc::from_path("dummy.sql").flatten_multiline_with(" . ");
        let b4 = SqlDoc::from_path("dummy.sql").flatten_multiline_with("--").preserve_multiline();
        assert!(matches!(b1, SqlDocBuilder { multiline_flat: MultiFlatten::NoFlat, .. }));
        assert!(matches!(b2, SqlDocBuilder { multiline_flat: MultiFlatten::FlattenWithNone, .. }));
        assert!(
            matches!(b3, SqlDocBuilder { multiline_flat: MultiFlatten::Flatten(s) , .. } if s == " . ")
        );
        assert!(matches!(b4, SqlDocBuilder { multiline_flat: MultiFlatten::NoFlat, .. }));
    }

    #[test]
    fn test_flatten_lines_behavior() {
        use crate::sql_doc::flatten_lines;
        let input = vec!["a", "b", "c"];
        let no_sep = flatten_lines(input.clone().into_iter(), "");
        assert_eq!(no_sep, "abc");
        let dash_sep = flatten_lines(input.clone().into_iter(), " - ");
        assert_eq!(dash_sep, "a - b - c");
        let single = flatten_lines(["solo"].into_iter(), "XXX");
        assert_eq!(single, "solo");
    }

    #[test]
    fn test_flatten_docs_no_flatter() {
        let table = TableDoc::new(
            None,
            "t".into(),
            Some("line1\nline2".into()),
            vec![
                ColumnDoc::new("c1".into(), Some("a\nb".into())),
                ColumnDoc::new("c2".into(), None),
            ],
            None,
        );
        let flat_table = TableDoc::new(
            None,
            "t".into(),
            Some("line1line2".into()),
            vec![ColumnDoc::new("c1".into(), Some("ab".into())), ColumnDoc::new("c2".into(), None)],
            None,
        );

        let output = {
            use crate::sql_doc::flatten_docs;
            let original = SqlDoc::new(vec![table]);
            flatten_docs(original, None)
        };
        assert_eq!(output.tables(), vec![flat_table], "NoFlat should not alter docs");
    }

    #[test]
    fn test_flatten_docs_flatten_no_separator() {
        let table = TableDoc::new(
            None,
            "t".into(),
            Some("A\nB\nC".into()),
            vec![ColumnDoc::new("c".into(), Some("x\ny".into()))],
            None,
        );
        let doc = SqlDoc::new(vec![table]);

        let out = {
            use crate::sql_doc::flatten_docs;
            flatten_docs(doc, Some(""))
        };

        let t = &out.tables()[0];

        assert_eq!(t.doc(), Some("ABC"));
        assert_eq!(t.columns()[0].doc(), Some("xy"));
    }

    #[test]
    fn test_flatten_docs_flatten_with_separator() {
        let table = TableDoc::new(
            None,
            "t".into(),
            Some("hello\nworld".into()),
            vec![ColumnDoc::new("c".into(), Some("x\ny\nz".into()))],
            None,
        );
        let doc = SqlDoc::new(vec![table]);

        let out = {
            use crate::sql_doc::flatten_docs;
            flatten_docs(doc, Some(" | "))
        };

        let t = &out.tables()[0];

        assert_eq!(t.doc(), Some("hello | world"));
        assert_eq!(t.columns()[0].doc(), Some("x | y | z"));
    }

    #[test]
    fn test_tables_mut_allows_modification() {
        let mut sql_doc =
            SqlDoc::new(vec![TableDoc::new(None, "t".into(), Some("old".into()), vec![], None)]);
        for t in sql_doc.tables_mut() {
            t.set_doc("new");
        }
        assert_eq!(sql_doc.tables()[0].doc(), Some("new"));
    }

    #[test]
    fn test_builder_build_with_flattening() -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("builder_flatten_test");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let file = base.join("t.sql");

        let sql = r"
            /* Table Doc line1
               line2 */
            CREATE TABLE things (
                /* col1
                   doc */
                id INTEGER
            );
        ";

        fs::write(&file, sql)?;

        let built1 = SqlDoc::from_path(&file).flatten_multiline_with(" • ").build()?;
        let built2 = SqlDoc::from_path(&file).flatten_multiline().build()?;
        let t1 = &built1.tables()[0];
        let t2 = &built2.tables()[0];
        assert_eq!(t1.doc(), Some("Table Doc line1 • line2"));
        assert_eq!(t1.columns()[0].doc(), Some("col1 • doc"));

        assert_eq!(t2.doc(), Some("Table Doc line1line2"));
        assert_eq!(t2.columns()[0].doc(), Some("col1doc"));
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }
}
