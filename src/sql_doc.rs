//! Public entry point for building [`SqlDoc`] from a directory, file, or string.

use std::{
    path::{Path, PathBuf},
    str::FromStr,
    vec,
};

use crate::{
    ast::ParsedSqlFile,
    comments::Comments,
    docs::{SqlFileDoc, TableDoc},
    error::DocError,
    files::{SqlFile, SqlFilesList},
};

/// Top-level documentation object containing all discovered [`TableDoc`] entries.
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
    Files(Vec<PathBuf>),
    FromString(&'a str),
    FromStringsWithPaths(&'a [(String, PathBuf)]),
}

impl SqlDoc {
    /// Method for creating a new [`SqlDoc`]
    #[must_use]
    pub fn new(mut tables: Vec<TableDoc>) -> Self {
        tables.sort_by(|a, b| a.name().cmp(b.name()));
        Self { tables }
    }

    /// Method for generating builder from a directory.
    pub fn from_dir<P: AsRef<Path>>(root: &P) -> SqlDocBuilder<'_> {
        SqlDocBuilder {
            source: SqlFileDocSource::Dir(root.as_ref().to_path_buf()),
            deny: Vec::new(),
            multiline_flat: MultiFlatten::NoFlat,
        }
    }
    /// Method for generating builder from a [`Path`] of a single file
    pub fn from_path<P: AsRef<Path> + ?Sized>(path: &P) -> SqlDocBuilder<'_> {
        SqlDocBuilder {
            source: SqlFileDocSource::File(path.as_ref().to_path_buf()),
            deny: Vec::new(),
            multiline_flat: MultiFlatten::NoFlat,
        }
    }

    /// Method for generating builder from a [`[Path]`]
    pub fn from_paths<P: AsRef<Path>>(paths: &[P]) -> SqlDocBuilder<'_> {
        SqlDocBuilder {
            source: SqlFileDocSource::Files(
                paths.iter().map(|p| p.as_ref().to_path_buf()).collect(),
            ),
            deny: Vec::new(),
            multiline_flat: MultiFlatten::NoFlat,
        }
    }

    /// Creates a builder from SQL text (no filesystem path is associated) from a [`str`]
    #[must_use]
    pub const fn builder_from_str(content: &str) -> SqlDocBuilder<'_> {
        SqlDocBuilder {
            source: SqlFileDocSource::FromString(content),
            deny: Vec::new(),
            multiline_flat: MultiFlatten::NoFlat,
        }
    }

    /// Creates a builder from a vec of tuples containing the `sql` as [`String`] and the path as [`PathBuf`]
    #[must_use]
    pub const fn builder_from_strs_with_paths(
        string_with_path: &[(String, PathBuf)],
    ) -> SqlDocBuilder<'_> {
        SqlDocBuilder {
            source: SqlFileDocSource::FromStringsWithPaths(string_with_path),
            deny: Vec::new(),
            multiline_flat: MultiFlatten::NoFlat,
        }
    }

    /// Method for finding a specific [`TableDoc`] by `name`
    ///
    /// # Parameters
    /// - the table `name` as a [`str`]
    /// - the table schema as `Option` of [`str`]
    ///
    /// # Errors
    /// - Will return [`DocError::TableNotFound`] if the expected table is not found
    /// - Will return [`DocError::TableWithSchemaNotFound`] if the table name exists but no table matches the given schema
    /// - Will return [`DocError::DuplicateTablesFound`] if more than one matching table is found
    pub fn table(&self, name: &str, schema: Option<&str>) -> Result<&TableDoc, DocError> {
        let tables = self.tables();
        let start = tables.partition_point(|n| n.name() < name);
        if start == tables.len() || tables[start].name() != name {
            return Err(DocError::TableNotFound { name: name.to_owned() });
        }
        let end = tables.partition_point(|t| t.name() <= name);
        match &tables[start..end] {
            [single] => Ok(single),
            multiple => {
                let mut schemas = multiple.iter().filter(|v| v.schema() == schema);
                let first = schemas.next().ok_or_else(|| DocError::TableWithSchemaNotFound {
                    name: name.to_owned(),
                    schema: schema.map_or_else(
                        || "No schema provided".to_owned(),
                        std::borrow::ToOwned::to_owned,
                    ),
                })?;
                if schemas.next().is_some() {
                    return Err(DocError::DuplicateTablesFound {
                        tables: multiple
                            .iter()
                            .filter(|v| v.schema() == schema)
                            .map(std::borrow::ToOwned::to_owned)
                            .collect(),
                    });
                }
                Ok(first)
            }
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

impl FromStr for SqlDoc {
    type Err = DocError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::builder_from_str(s).build()
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

    /// Method for flattening the multiline comments without additional formatting
    #[must_use]
    pub fn flatten_multiline(mut self) -> Self {
        self.multiline_flat = MultiFlatten::FlattenWithNone;
        self
    }

    /// Method for flattening the multiline comments with [`String`] containing additional leading line formatting to add, such as punctuation.
    #[must_use]
    pub fn flatten_multiline_with(mut self, suffix: &str) -> Self {
        self.multiline_flat = MultiFlatten::Flatten(suffix.to_owned());
        self
    }
    /// Method to set multiline comments to preserve multiple lines
    #[must_use]
    pub fn preserve_multiline(mut self) -> Self {
        self.multiline_flat = MultiFlatten::NoFlat;
        self
    }
    /// Builds the [`SqlDoc`]
    ///
    ///
    /// Comment flattening (if enabled) is applied as a post-processing step after docs are generated.
    ///
    /// # Errors
    /// - Will return `DocError` bubbled up
    pub fn build(self) -> Result<SqlDoc, DocError> {
        let docs: Vec<SqlFileDoc> = match &self.source {
            SqlFileDocSource::Dir(path) => generate_docs_from_dir(path, &self.deny)?,
            SqlFileDocSource::File(file) => {
                let sql_doc = generate_docs_from_file(file)?;
                vec![sql_doc]
            }
            SqlFileDocSource::FromString(content) => {
                let sql_docs = generate_docs_str(content, None)?;
                vec![sql_docs]
            }
            SqlFileDocSource::FromStringsWithPaths(strings_paths) => {
                generate_docs_from_strs_with_paths(strings_paths)?
            }
            SqlFileDocSource::Files(files) => generate_docs_from_files(files)?,
        };
        let num_of_tables = docs.iter().map(super::docs::SqlFileDoc::number_of_tables).sum();
        let mut tables = Vec::with_capacity(num_of_tables);
        for sql_doc in docs {
            tables.extend(sql_doc);
        }
        let mut sql_doc = SqlDoc::new(tables);
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
    let deny_list: Vec<String> = deny.iter().map(|file| file.as_ref().to_owned()).collect();
    let file_set = SqlFilesList::new(source, &deny_list)?;
    let mut sql_docs = Vec::new();
    for file in file_set.sql_files() {
        let docs = generate_docs_from_file(file)?;
        sql_docs.push(docs);
    }
    Ok(sql_docs)
}

fn generate_docs_from_files(files: &[PathBuf]) -> Result<Vec<SqlFileDoc>, DocError> {
    let mut sql_docs = Vec::new();
    for file in files {
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

fn generate_docs_str(content: &str, path: Option<PathBuf>) -> Result<SqlFileDoc, DocError> {
    let dummy_file = SqlFile::new_from_str(content.to_owned(), path);
    let parsed_sql = ParsedSqlFile::parse(dummy_file)?;
    let comments = Comments::parse_all_comments_from_file(&parsed_sql)?;
    let docs = SqlFileDoc::from_parsed_file(&parsed_sql, &comments)?;
    Ok(docs)
}

fn generate_docs_from_strs_with_paths(
    strings_with_paths: &[(String, PathBuf)],
) -> Result<Vec<SqlFileDoc>, DocError> {
    let mut docs = Vec::new();
    for (content, path) in strings_with_paths {
        docs.push(generate_docs_str(content, Some(path.to_owned()))?);
    }

    Ok(docs)
}

#[cfg(test)]
mod tests {
    use std::{
        env, fs,
        path::{Path, PathBuf},
        vec,
    };

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
        let mut expected_tables: Vec<TableDoc> =
            expected.into_iter().flat_map(SqlDoc::into_tables).collect();
        stamp_table_paths(&mut expected_tables, &file);
        let expected_doc = SqlDoc::new(expected_tables);
        assert_eq!(sql_doc, expected_doc);
        let names: Vec<&str> =
            sql_doc.tables().iter().map(super::super::docs::TableDoc::name).collect();
        let mut sorted = names.clone();
        sorted.sort_unstable();
        assert_eq!(names, sorted, "tables should be in alphabetical order");
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }
    #[test]
    fn build_sql_doc_from_dir() -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("build_sql_doc_from_dir");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let mut expected: Vec<TableDoc> = Vec::new();
        for (idx, (contents, doc)) in sample_sql().into_iter().enumerate() {
            let path = base.join(format!("test_file{idx}.sql"));
            fs::write(&path, contents)?;
            let mut tables = doc.into_tables();
            stamp_table_paths(&mut tables, &path);
            expected.extend(tables);
        }
        let sql_doc = SqlDoc::from_dir(&base).build()?;
        let mut actual: Vec<TableDoc> = sql_doc.into_tables();
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
        let mut expected_tables: Vec<TableDoc> =
            expected.into_iter().flat_map(SqlDoc::into_tables).collect();
        stamp_table_paths(&mut expected_tables, &file);
        let expected_doc = SqlDoc::new(expected_tables);
        assert_eq!(sql_doc, expected_doc);
        let table = "users";
        assert_eq!(sql_doc.table(table, None)?, expected_doc.table(table, None)?);
        let schema = "analytics";
        let schema_table = "events";
        assert_eq!(
            sql_doc.table(schema_table, Some(schema))?,
            expected_doc.table(schema_table, Some(schema))?
        );
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }

    #[test]
    fn test_table_err() {
        let empty_set = SqlDoc::new(vec![]);
        let empty_table_err = empty_set.table("name", None);
        assert!(empty_table_err.is_err());
        assert!(matches!(
            empty_table_err,
            Err(DocError::TableNotFound { name }) if name == "name"
        ));
    }

    #[test]
    fn test_schema_err() {
        let empty_set = SqlDoc::new(vec![]);
        let empty_table_err = empty_set.table("name", Some("schema"));
        assert!(empty_table_err.is_err());
        assert!(matches!(
            empty_table_err,
            Err(DocError::TableNotFound { name }) if name == "name"
        ));
        let duplicate_set = SqlDoc::new(vec![
            TableDoc::new(Some("schema".to_owned()), "duplicate".to_owned(), None, vec![], None),
            TableDoc::new(Some("schema".to_owned()), "duplicate".to_owned(), None, vec![], None),
        ]);
        let duplicate_tables_err = duplicate_set.table("duplicate", Some("schema"));
        assert!(matches!(duplicate_tables_err, Err(DocError::DuplicateTablesFound { .. })));
    }

    fn sort_tables(tables: &mut [TableDoc]) {
        tables.sort_by(|a, b| {
            let a_key = (a.schema().unwrap_or(""), a.name());
            let b_key = (b.schema().unwrap_or(""), b.name());
            a_key.cmp(&b_key)
        });
    }

    fn stamp_table_paths(tables: &mut [TableDoc], path: &Path) {
        let pb = path.to_path_buf();
        for t in tables {
            t.set_path(Some(pb.clone()));
        }
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
                    "users".to_owned(),
                    Some("Users table".to_owned()),
                    vec![
                        ColumnDoc::new("id".to_owned(), Some("id".to_owned())),
                        ColumnDoc::new("username".to_owned(), Some("login name".to_owned())),
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
                    "posts".to_owned(),
                    Some("Posts table".to_owned()),
                    vec![
                        ColumnDoc::new("id".to_owned(), Some("primary key".to_owned())),
                        ColumnDoc::new("title".to_owned(), None),
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
                    "things".to_owned(),
                    None,
                    vec![
                        ColumnDoc::new("id".to_owned(), None),
                        ColumnDoc::new("name".to_owned(), None),
                        ColumnDoc::new("value".to_owned(), None),
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
                    Some("analytics".to_owned()),
                    "events".to_owned(),
                    Some("Table with schema".to_owned()),
                    vec![
                        ColumnDoc::new("id".to_owned(), Some("event id".to_owned())),
                        ColumnDoc::new("payload".to_owned(), Some("event payload".to_owned())),
                    ],
                    None,
                )]),
            ),
        ]
    }

    #[test]
    fn test_sql_doc_getters() {
        let tables = vec![TableDoc::new(None, "name".to_owned(), None, vec![], None)];
        let sql_doc = SqlDoc::new(vec![TableDoc::new(None, "name".to_owned(), None, vec![], None)]);
        assert_eq!(sql_doc.tables().len(), 1);
        assert_eq!(sql_doc.tables(), tables);
    }

    #[test]
    fn test_sql_builder_deny_from_path() {
        let actual_builder = SqlDoc::from_path("path").deny("path1").deny("path2");
        let expected_builder = SqlDocBuilder {
            source: crate::sql_doc::SqlFileDocSource::File(PathBuf::from("path")),
            deny: vec!["path1".to_owned(), "path2".to_owned()],
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
        let sql_doc = SqlDoc::from_path(&file).build()?;
        let deny_str =
            file.to_str().unwrap_or_else(|| panic!("expected a file from PathBuf Found None"));
        let sql_doc_deny = SqlDoc::from_dir(&base).deny(deny_str).build()?;
        let mut expected_tables: Vec<TableDoc> =
            expected.into_iter().flat_map(SqlDoc::into_tables).collect();
        stamp_table_paths(&mut expected_tables, &file);
        let expected_doc = SqlDoc::new(expected_tables);
        assert_eq!(sql_doc, expected_doc);
        assert_eq!(sql_doc_deny, SqlDoc::new(vec![]));
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
    #[test]
    fn test_sql_doc_from_str_builds_expected_builder() {
        let content = "CREATE TABLE t(id INTEGER);";

        let actual = SqlDoc::builder_from_str(content);

        let expected = SqlDocBuilder {
            source: crate::sql_doc::SqlFileDocSource::FromString(content),
            deny: vec![],
            multiline_flat: MultiFlatten::NoFlat,
        };

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_from_str_parse_sql_doc() -> Result<(), Box<dyn std::error::Error>> {
        let doc: SqlDoc = "CREATE TABLE t(id INTEGER);".parse()?;
        assert_eq!(doc.tables().len(), 1);
        Ok(())
    }

    #[test]
    fn test_build_sql_doc_from_paths() -> Result<(), Box<dyn std::error::Error>> {
        let base = env::temp_dir().join("build_sql_doc_from_paths");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let sample = sample_sql();
        let (sql1, doc1) = &sample[0];
        let (sql2, doc2) = &sample[1];

        let file1 = base.join("one.sql");
        let file2 = base.join("two.sql");
        fs::write(&file1, sql1)?;
        fs::write(&file2, sql2)?;

        let paths = vec![file1.clone(), file2.clone()];
        let sql_doc = SqlDoc::from_paths(&paths).build()?;

        let mut expected_tables: Vec<TableDoc> = Vec::new();

        let mut t1 = doc1.clone().into_tables();
        stamp_table_paths(&mut t1, &file1);
        expected_tables.extend(t1);

        let mut t2 = doc2.clone().into_tables();
        stamp_table_paths(&mut t2, &file2);
        expected_tables.extend(t2);

        let mut actual_tables = sql_doc.into_tables();
        assert_eq!(actual_tables.len(), expected_tables.len());

        sort_tables(&mut actual_tables);
        sort_tables(&mut expected_tables);

        assert_eq!(actual_tables, expected_tables);

        let _ = fs::remove_dir_all(&base);
        Ok(())
    }

    #[test]
    fn test_tables_binary_searchable_by_name() {
        let sample = sample_sql();
        let tables: Vec<TableDoc> =
            sample.into_iter().flat_map(|(_, doc)| doc.into_tables()).collect();
        let sql_doc = SqlDoc::new(tables);
        let id = sql_doc
            .tables()
            .binary_search_by(|t| t.name().cmp("users"))
            .unwrap_or_else(|_| panic!("expected to find table `users` via binary search"));
        assert_eq!(sql_doc.tables()[id].name(), "users");
    }

    #[test]
    fn test_table_with_schema_not_found_when_name_exists() {
        let sql_doc = SqlDoc::new(vec![
            TableDoc::new(Some("analytics".to_owned()), "events".to_owned(), None, vec![], None),
            TableDoc::new(Some("public".to_owned()), "events".to_owned(), None, vec![], None),
        ]);

        match sql_doc.table("events", Some("missing")) {
            Err(DocError::TableWithSchemaNotFound { name, schema })
                if name == "events" && schema == "missing" => {}
            Err(e) => panic!("expected TableWithSchemaNotFound(events, missing), got: {e:?}"),
            Ok(_) => panic!("expected error, got Ok"),
        }
    }

    #[test]
    fn test_table_duplicate_tables_found_for_same_name_and_schema() {
        let sql_doc = SqlDoc::new(vec![
            TableDoc::new(Some("analytics".to_owned()), "events".to_owned(), None, vec![], None),
            TableDoc::new(Some("analytics".to_owned()), "events".to_owned(), None, vec![], None),
        ]);

        match sql_doc.table("events", Some("analytics")) {
            Err(DocError::DuplicateTablesFound { .. }) => {}
            Err(e) => panic!("expected DuplicateTablesFound, got: {e:?}"),
            Ok(_) => panic!("expected error, got Ok"),
        }
    }

    #[test]
    fn test_table_selects_correct_schema_when_multiple_exist()
    -> Result<(), Box<dyn std::error::Error>> {
        let sql_doc = SqlDoc::new(vec![
            TableDoc::new(Some("analytics".to_owned()), "events".to_owned(), None, vec![], None),
            TableDoc::new(Some("public".to_owned()), "events".to_owned(), None, vec![], None),
        ]);

        let t = sql_doc.table("events", Some("public"))?;
        assert_eq!(t.schema(), Some("public"));
        Ok(())
    }
    #[test]
    fn test_generate_docs_from_strs_with_paths_builds_tables_and_stamps_paths()
    -> Result<(), Box<dyn std::error::Error>> {
        // Two simple SQL strings with distinct paths
        let sql1 = r#"
            -- Users table
            CREATE TABLE users (
                -- id
                id INTEGER PRIMARY KEY
            );
        "#;

        let sql2 = r#"
            /* Posts table */
            CREATE TABLE posts (
                /* primary key */
                id INTEGER PRIMARY KEY
            );
        "#;

        let p1 = PathBuf::from("a/one.sql");
        let p2 = PathBuf::from("b/two.sql");

        // NOTE: builder expects owned String for sql and a PathBuf
        let inputs: Vec<(String, PathBuf)> =
            vec![(sql1.to_owned(), p1.clone()), (sql2.to_owned(), p2.clone())];

        // Build via the new builder arm
        let doc = SqlDoc::builder_from_strs_with_paths(&inputs).build()?;

        // We should have 2 tables total
        assert_eq!(doc.tables().len(), 2);

        // Verify table names exist
        let users = doc.table("users", None)?;
        let posts = doc.table("posts", None)?;

        // Verify each table got the correct stamped path
        assert_eq!(users.path(), Some(p1.as_path()));
        assert_eq!(posts.path(), Some(p2.as_path()));

        Ok(())
    }

    #[test]
    fn test_builder_from_strs_with_paths_is_used_in_build_match_arm()
    -> Result<(), Box<dyn std::error::Error>> {
        let sql_a = "CREATE TABLE alpha (id INTEGER);";
        let sql_b = "CREATE TABLE beta (id INTEGER);";
        let path_a = PathBuf::from("alpha.sql");
        let path_b = PathBuf::from("beta.sql");

        let inputs = vec![(sql_a.to_owned(), path_a.clone()), (sql_b.to_owned(), path_b.clone())];

        let built = SqlDoc::builder_from_strs_with_paths(&inputs).build()?;

        let names: Vec<&str> = built.tables().iter().map(|t| t.name()).collect();
        assert_eq!(names, vec!["alpha", "beta"]);

        assert_eq!(built.table("alpha", None)?.path(), Some(path_a.as_path()));
        assert_eq!(built.table("beta", None)?.path(), Some(path_b.as_path()));

        Ok(())
    }

    #[test]
    fn test_builder_from_str_no_path_has_none_path() -> Result<(), Box<dyn std::error::Error>> {
        let sql = "CREATE TABLE t (id INTEGER);";
        let built = SqlDoc::builder_from_str(sql).build()?;

        let t = built.table("t", None)?;
        assert_eq!(t.path(), None);

        Ok(())
    }
    #[test]
fn test_table_with_schema_not_found_uses_no_schema_provided_message() {
    use crate::{SqlDoc, docs::TableDoc, error::DocError};

    let sql_doc = SqlDoc::new(vec![
        TableDoc::new(Some("analytics".to_owned()), "events".to_owned(), None, vec![], None),
        TableDoc::new(Some("public".to_owned()), "events".to_owned(), None, vec![], None),
    ]);

    match sql_doc.table("events", None) {
        Err(DocError::TableWithSchemaNotFound { name, schema }) => {
            assert_eq!(name, "events");
            assert_eq!(schema, "No schema provided");
        }
        Err(e) => panic!(
            "expected TableWithSchemaNotFound with 'No schema provided', got: {e:?}"
        ),
        Ok(_) => panic!("expected error, got Ok"),
    }
}

}
