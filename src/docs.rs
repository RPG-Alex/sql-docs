//! Convert parsed SQL + extracted comments into structured documentation types.

use core::fmt;
use std::path::{Path, PathBuf};

use sqlparser::ast::{Ident, ObjectName, ObjectNamePart, Spanned, Statement};

use crate::{ast::ParsedSqlFile, comments::Comments, error::DocError};

/// Structure for containing the `name` of the `Column` and an [`Option`] for
/// the comment as a [`String`]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ColumnDoc {
    name: String,
    doc: Option<String>,
}
impl ColumnDoc {
    /// Creates a new [`ColumnDoc`]
    ///
    /// # Parameters
    /// - name: `String` - the name of the column
    /// - doc: `Option<String>` the comment for the column
    #[must_use]
    pub const fn new(name: String, doc: Option<String>) -> Self {
        Self { name, doc }
    }

    /// Getter for the `name` field
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Getter for the field `doc`
    #[must_use]
    pub fn doc(&self) -> Option<&str> {
        self.doc.as_deref()
    }

    /// Setter to update the table doc
    pub fn set_doc(&mut self, doc: impl Into<String>) {
        self.doc = Some(doc.into());
    }
}

impl fmt::Display for ColumnDoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Column Name: {}", self.name())?;
        if let Some(c) = self.doc() {
            writeln!(f, "Column Doc: {c}")?;
        } else {
            writeln!(f, "No Column Doc Found")?;
        }
        Ok(())
    }
}

/// Structure for containing the `name` of the `Table`, an [`Option`] for if the
/// table has a schema,  an [`Option`] for the comment as a [`String`], and a
/// `Vec` of [`ColumnDoc`] contained in the table
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TableDoc {
    schema: Option<String>,
    name: String,
    doc: Option<String>,
    columns: Vec<ColumnDoc>,
    path: Option<PathBuf>,
}

impl TableDoc {
    /// Creates a new [`TableDoc`] after sorting [`ColumnDoc`] by name
    ///
    /// # Parameters
    /// - name: `String` - the name of the table
    /// - doc: `Option<String>` of the comment for table
    /// - columns: the `Vec<ColumnDoc>` of all [`ColumnDoc`] for this table
    #[must_use]
    #[allow(clippy::missing_const_for_fn)]
    pub fn new(
        schema: Option<String>,
        name: String,
        doc: Option<String>,
        mut columns: Vec<ColumnDoc>,
        path: Option<PathBuf>,
    ) -> Self {
        columns.sort_by(|a,b| a.name().cmp(b.name()));
        Self { schema, name, doc, columns, path }
    }

    /// Getter for the `Schema` of the table (if there is one)
    #[must_use]
    pub fn schema(&self) -> Option<&str> {
        self.schema.as_deref()
    }

    /// Getter for the `name` field
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Getter for the `doc` field
    #[must_use]
    pub fn doc(&self) -> Option<&str> {
        self.doc.as_deref()
    }

    /// Setter to update the table doc
    pub fn set_doc(&mut self, doc: impl Into<String>) {
        self.doc = Some(doc.into());
    }

    /// Setter for updating the table [`PathBuf`] source
    pub fn set_path(&mut self, path: Option<impl Into<PathBuf>>) {
        self.path = path.map(Into::into);
    }

    /// Getter for the `columns` field
    #[must_use]
    pub fn columns(&self) -> &[ColumnDoc] {
        &self.columns
    }

    /// Getter that returns a mutable reference to the [`ColumnDoc`] vec
    pub fn columns_mut(&mut self) -> &mut [ColumnDoc] {
        &mut self.columns
    }

    /// Method for finding a specific [`ColumnDoc`] from `schema` and table `name`
    ///
    /// # Parameters
    /// - the column's `name` as a [`str`]
    ///
    /// # Errors
    /// - Will return [`DocError::ColumnNotFound`] if the expected table is not found
    pub fn column(&self, column: &str) -> Result<&ColumnDoc, DocError> {
        let matches = self
            .columns
            .iter()
            .filter(|col_doc| col_doc.name() == column)
            .collect::<Vec<&ColumnDoc>>();
        match matches.as_slice() {
            [] => Err(DocError::ColumnNotFound { name: column.to_owned() }),
            [only] => Ok(*only),
            _ => Err(DocError::DuplicateColumnsFound {
                columns: matches.into_iter().cloned().collect(),
            }),
        }
    }

    /// Getter method for retrieving the table's [`Path`]
    #[must_use]
    pub fn path(&self) -> Option<&Path> {
        self.path.as_deref()
    }
}

impl fmt::Display for TableDoc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(s) = self.schema() {
            writeln!(f, "Table Schema: {s}")?;
        } else {
            writeln!(f, "No Table Schema")?;
        }

        writeln!(f, "Table Name: {}", self.name())?;

        if let Some(d) = self.doc() {
            writeln!(f, "Table Doc: {d}")?;
        } else {
            writeln!(f, "No Table Doc")?;
        }

        writeln!(f, "Table Column Docs: ")?;
        for col in self.columns() {
            write!(f, " {col}")?;
        }
        Ok(())
    }
}

/// Structure for containing the docs for every `Table` in an `.sql` file as a
/// `Vec` of [`TableDoc`]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SqlFileDoc {
    tables: Vec<TableDoc>,
}

impl SqlFileDoc {
    /// Create a new instance of [`SqlFileDoc`]
    ///
    /// # Parameters
    /// - `tables` the `Vec` of [`TableDoc`] for the struct
    #[must_use]
    pub const fn new(tables: Vec<TableDoc>) -> Self {
        Self { tables }
    }

    /// Final structured documentation extracted from one SQL file.
    ///
    /// This merges:
    /// - Parsed SQL AST (`CREATE TABLE` statements for example)
    /// - Comment spans into a format suitable for documentation generation.
    ///
    /// # Parameters
    /// - `file`: the [`ParsedSqlFile`]
    /// - `comments`: the parsed [`Comments`]
    ///
    /// # Errors
    /// - Returns [`DocError::InvalidObjectName`] if the table name has no identifier components.
    /// - May also propagate other [`DocError`] variants from lower layers in the future.
    pub fn from_parsed_file(file: &ParsedSqlFile, comments: &Comments) -> Result<Self, DocError> {
        let mut tables = Vec::new();
        for statement in file.statements() {
            #[allow(clippy::single_match)]
            match statement {
                Statement::CreateTable(table) => {
                    let table_start = table.span().start.line;
                    let mut column_docs = Vec::new();
                    for column in &table.columns {
                        let column_start = column.span().start.line;
                        let column_leading = comments.leading_comment(column_start);
                        let column_name = column.name.value.clone();
                        let column_doc = match column_leading {
                            Some(col_comment) => {
                                ColumnDoc::new(column_name, Some(col_comment.text().to_owned()))
                            }
                            None => ColumnDoc::new(column_name, None),
                        };
                        column_docs.push(column_doc);
                    }
                    let table_leading = comments.leading_comment(table_start);
                    let (schema, name) = schema_and_table(&table.name)?;
                    let table_doc = TableDoc::new(
                        schema,
                        name,
                        table_leading.as_ref().map(|c| c.text().to_owned()),
                        column_docs,
                        file.path_into_path_buf(),
                    );
                    tables.push(table_doc);
                }
                // can add support for other types of statements below
                _ => {}
            }
        }

        Ok(Self { tables })
    }

    /// Getter function to get a slice of [`TableDoc`]
    #[must_use]
    pub fn tables(&self) -> &[TableDoc] {
        &self.tables
    }

    /// Getter that returns a mutable reference to the [`TableDoc`] vec
    pub fn tables_mut(&mut self) -> &mut [TableDoc] {
        &mut self.tables
    }

    /// Returns the number fo tables in the `SqlFileDoc`
    #[must_use]
    pub fn number_of_tables(&self) -> usize {
        self.tables().len()
    }
}

/// Converts a file doc into its table docs (consumes the [`SqlFileDoc`]).
impl From<SqlFileDoc> for Vec<TableDoc> {
    fn from(value: SqlFileDoc) -> Self {
        value.tables
    }
}

impl IntoIterator for SqlFileDoc {
    type Item = TableDoc;
    type IntoIter = <Vec<TableDoc> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.tables.into_iter()
    }
}

/// Helper function that will parse the table's schema and table name.
/// Easily extensible for catalog if neeeded as well.
///
/// # Parameters
/// - `name` the [`ObjectName`] structure for the statement
///
/// # Errors
/// - [`DocError`] will return the location of the statement if there is a statement without a schema and table name.
fn schema_and_table(name: &ObjectName) -> Result<(Option<String>, String), DocError> {
    let idents: Vec<&Ident> = name
        .0
        .iter()
        .filter_map(|part| match part {
            ObjectNamePart::Identifier(ident) => Some(ident),
            ObjectNamePart::Function(_func) => None,
        })
        .collect();

    match idents.as_slice() {
        [] => {
            let span = name.span();
            Err(DocError::InvalidObjectName {
                message: "ObjectName had no identifier parts".to_owned(),
                line: span.start.line,
                column: span.start.column,
            })
        }
        [only] => Ok((None, only.value.clone())),
        [.., schema, table] => Ok((Some(schema.value.clone()), table.value.clone())),
    }
}

#[cfg(test)]
mod tests {
    use core::fmt;
    use std::{env, fs, path::PathBuf};

    use sqlparser::{
        ast::{Ident, ObjectName, ObjectNamePart, ObjectNamePartFunction},
        tokenizer::Span,
    };

    use crate::{
        docs::{ColumnDoc, SqlFileDoc, TableDoc, schema_and_table},
        error::DocError,
    };

    #[test]
    fn test_sql_docs_struct() {
        let column_doc = ColumnDoc::new("id".to_owned(), Some("The ID for the table".to_owned()));
        let columns = vec![column_doc];
        let table_doc = TableDoc::new(
            None,
            "user".to_owned(),
            Some("The table for users".to_owned()),
            columns,
            None,
        );
        let tables = vec![table_doc];
        let sql_doc = SqlFileDoc::new(tables);
        let sql_doc_val =
            sql_doc.tables().first().unwrap_or_else(|| panic!("unable to find table"));
        assert_eq!(sql_doc_val.name(), "user");
        let sql_doc_val_column =
            sql_doc_val.columns().first().unwrap_or_else(|| panic!("unable to find columns"));
        assert_eq!(sql_doc_val_column.name(), "id");
    }

    fn single_line_comments_sql() -> &'static str {
        "-- Users table stores user account information
CREATE TABLE users (
    -- Primary key
    id INTEGER PRIMARY KEY,
    -- Username for login
    username VARCHAR(255) NOT NULL,
    -- Email address
    email VARCHAR(255) UNIQUE NOT NULL,
    -- When the user registered
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Posts table stores blog posts
CREATE TABLE posts (
    -- Primary key
    id INTEGER PRIMARY KEY,
    -- Post title
    title VARCHAR(255) NOT NULL,
    -- Foreign key linking to users
    user_id INTEGER NOT NULL,
    -- Main body text
    body TEXT NOT NULL,
    -- When the post was created
    published_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);"
    }

    fn multiline_comments_sql() -> &'static str {
        r"/* Users table stores user account information 
multiline */
CREATE TABLE users (
    /* Primary key 
    multiline */
    id INTEGER PRIMARY KEY,
    /* Username for login 
    multiline */
    username VARCHAR(255) NOT NULL,
    /* Email address 
    multiline */
    email VARCHAR(255) UNIQUE NOT NULL,
    /* When the user registered 
    multiline */
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

/* Posts table stores blog posts 
multiline */
CREATE TABLE posts (
    /* Primary key 
    multiline */
    id INTEGER PRIMARY KEY,
    /* Post title 
    multiline */
    title VARCHAR(255) NOT NULL,
    /* Foreign key linking to users 
    multiline */
    user_id INTEGER NOT NULL,
    /* Main body text 
    multiline */
    body TEXT NOT NULL,
    /* When the post was created 
    multiline */
    published_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);"
    }

    fn no_comments_sql() -> &'static str {
        "CREATE TABLE users (
    id INTEGER PRIMARY KEY,
    username VARCHAR(255) NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE posts (
    id INTEGER PRIMARY KEY,
    title VARCHAR(255) NOT NULL,
    user_id INTEGER NOT NULL,
    body TEXT NOT NULL,
    published_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);"
    }

    fn mixed_comments_sql() -> &'static str {
        "-- interstitial Comment above statements (should be ignored)

/* Users table stores user account information */
CREATE TABLE users ( /* users interstitial comment 
(should be ignored) */
    -- Primary key
    id INTEGER PRIMARY KEY, -- Id comment that is interstitial (should be ignored)
    /* Username for login */
    username VARCHAR(255) NOT NULL,
    -- Email address
    email VARCHAR(255) UNIQUE NOT NULL,
    /* When the user registered */
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

/* Posts table stores blog posts */
CREATE TABLE posts (
    -- Primary key
    id INTEGER PRIMARY KEY,
    /* Post title */
    title VARCHAR(255) NOT NULL,
    -- Foreign key linking to users
    user_id INTEGER NOT NULL,
    /* Main body text */
    body TEXT NOT NULL,
    -- When the post was created
    published_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
"
    }

    #[test]
    fn generate_docs_files() -> Result<(), Box<dyn std::error::Error>> {
        use crate::{ast::ParsedSqlFileSet, comments::Comments, files::SqlFileSet};
        let base = env::temp_dir().join("all_sql_files2");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let file1 = base.join("with_single_line_comments.sql");
        fs::File::create(&file1)?;
        fs::write(&file1, single_line_comments_sql())?;
        let file2 = base.join("with_multiline_comments.sql");
        fs::File::create(&file2)?;
        fs::write(&file2, multiline_comments_sql())?;
        let file3 = base.join("with_mixed_comments.sql");
        fs::File::create(&file3)?;
        fs::write(&file3, mixed_comments_sql())?;
        let file4 = base.join("without_comments.sql");
        fs::File::create(&file4)?;
        fs::write(&file4, no_comments_sql())?;
        let set = SqlFileSet::new(&base, &[])?;
        let parsed_set = ParsedSqlFileSet::parse_all(set)?;
        let expected_values = expect_values();

        for file in parsed_set.files() {
            let comments = Comments::parse_all_comments_from_file(file)?;
            let docs = SqlFileDoc::from_parsed_file(file, &comments);
            let filename = file
                .file()
                .path()
                .and_then(|p| p.file_name())
                .and_then(|s| s.to_str())
                .ok_or("unable to parse file")?;

            let got = docs?;
            let file_path = file.file().path().ok_or("missing path")?;

            match filename {
                "with_single_line_comments.sql" | "with_mixed_comments.sql" => {
                    let expected = with_path(expected_values[0].clone(), file_path);
                    assert_eq!(&got, &expected);
                }
                "with_multiline_comments.sql" => {
                    let expected = with_path(expected_values[1].clone(), file_path);
                    assert_eq!(&got, &expected);
                }
                "without_comments.sql" => {
                    let expected = with_path(expected_without_comments_docs(), file_path);
                    assert_eq!(&got, &expected);
                }
                _ => unreachable!(),
            }
        }
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }

    fn with_path(mut doc: SqlFileDoc, path: &std::path::Path) -> SqlFileDoc {
        let pb = path.to_path_buf();

        for table in doc.tables_mut() {
            table.set_path(Some(pb.clone()));
        }

        doc
    }

    fn expected_without_comments_docs() -> SqlFileDoc {
        SqlFileDoc::new(vec![
            TableDoc::new(
                None,
                "users".to_owned(),
                None,
                vec![
                    ColumnDoc::new("id".to_owned(), None),
                    ColumnDoc::new("username".to_owned(), None),
                    ColumnDoc::new("email".to_owned(), None),
                    ColumnDoc::new("created_at".to_owned(), None),
                ],
                None,
            ),
            TableDoc::new(
                None,
                "posts".to_owned(),
                None,
                vec![
                    ColumnDoc::new("id".to_owned(), None),
                    ColumnDoc::new("title".to_owned(), None),
                    ColumnDoc::new("user_id".to_owned(), None),
                    ColumnDoc::new("body".to_owned(), None),
                    ColumnDoc::new("published_at".to_owned(), None),
                ],
                None,
            ),
        ])
    }

    fn expect_values() -> Vec<SqlFileDoc> {
        let mut docs = Vec::new();

        let first_docs = SqlFileDoc::new(vec![
            TableDoc::new(
                None,
                "users".to_owned(),
                Some("Users table stores user account information".to_owned()),
                vec![
                    ColumnDoc::new("id".to_owned(), Some("Primary key".to_owned())),
                    ColumnDoc::new("username".to_owned(), Some("Username for login".to_owned())),
                    ColumnDoc::new("email".to_owned(), Some("Email address".to_owned())),
                    ColumnDoc::new(
                        "created_at".to_owned(),
                        Some("When the user registered".to_owned()),
                    ),
                ],
                None,
            ),
            TableDoc::new(
                None,
                "posts".to_owned(),
                Some("Posts table stores blog posts".to_owned()),
                vec![
                    ColumnDoc::new("id".to_owned(), Some("Primary key".to_owned())),
                    ColumnDoc::new("title".to_owned(), Some("Post title".to_owned())),
                    ColumnDoc::new(
                        "user_id".to_owned(),
                        Some("Foreign key linking to users".to_owned()),
                    ),
                    ColumnDoc::new("body".to_owned(), Some("Main body text".to_owned())),
                    ColumnDoc::new(
                        "published_at".to_owned(),
                        Some("When the post was created".to_owned()),
                    ),
                ],
                None,
            ),
        ]);
        docs.push(first_docs);

        let second_docs = SqlFileDoc::new(vec![
            TableDoc::new(
                None,
                "users".to_owned(),
                Some("Users table stores user account information\nmultiline".to_owned()),
                vec![
                    ColumnDoc::new("id".to_owned(), Some("Primary key\nmultiline".to_owned())),
                    ColumnDoc::new(
                        "username".to_owned(),
                        Some("Username for login\nmultiline".to_owned()),
                    ),
                    ColumnDoc::new("email".to_owned(), Some("Email address\nmultiline".to_owned())),
                    ColumnDoc::new(
                        "created_at".to_owned(),
                        Some("When the user registered\nmultiline".to_owned()),
                    ),
                ],
                None,
            ),
            TableDoc::new(
                None,
                "posts".to_owned(),
                Some("Posts table stores blog posts\nmultiline".to_owned()),
                vec![
                    ColumnDoc::new("id".to_owned(), Some("Primary key\nmultiline".to_owned())),
                    ColumnDoc::new("title".to_owned(), Some("Post title\nmultiline".to_owned())),
                    ColumnDoc::new(
                        "user_id".to_owned(),
                        Some("Foreign key linking to users\nmultiline".to_owned()),
                    ),
                    ColumnDoc::new("body".to_owned(), Some("Main body text\nmultiline".to_owned())),
                    ColumnDoc::new(
                        "published_at".to_owned(),
                        Some("When the post was created\nmultiline".to_owned()),
                    ),
                ],
                None,
            ),
        ]);
        docs.push(second_docs);

        docs
    }

    #[test]
    fn test_doc() {
        let col_doc = ColumnDoc::new("test".to_owned(), Some("comment".to_owned()));
        assert_eq!(&col_doc.to_string(), &"Column Name: test\nColumn Doc: comment\n".to_owned());
        let col_doc_no_doc = ColumnDoc::new("test".to_owned(), None);
        assert_eq!(
            &col_doc_no_doc.to_string(),
            &"Column Name: test\nNo Column Doc Found\n".to_owned()
        );
        assert_eq!(col_doc.doc(), Some("comment"));
        assert_eq!(col_doc.name(), "test");
        assert_eq!(col_doc_no_doc.doc(), None);
        assert_eq!(col_doc_no_doc.name(), "test");
        let table_doc = TableDoc::new(
            Some("schema".to_owned()),
            "table".to_owned(),
            Some("table doc".to_owned()),
            vec![col_doc],
            None,
        );
        let table_doc_no_doc =
            TableDoc::new(None, "table".to_owned(), None, vec![col_doc_no_doc], None);
        assert_eq!(table_doc.name(), "table");
        assert_eq!(table_doc.schema(), Some("schema"));
        assert_eq!(
            table_doc.to_string(),
            "Table Schema: schema\nTable Name: table\nTable Doc: table doc\nTable Column Docs: \n Column Name: test\nColumn Doc: comment\n"
        );
        assert_eq!(table_doc_no_doc.schema(), None);
        assert_eq!(table_doc_no_doc.name(), "table");
        assert_eq!(
            table_doc_no_doc.to_string(),
            "No Table Schema\nTable Name: table\nNo Table Doc\nTable Column Docs: \n Column Name: test\nNo Column Doc Found\n"
        );
    }

    fn ident(v: &str) -> Ident {
        Ident { value: v.to_owned(), quote_style: None, span: Span::empty() }
    }

    fn func_part(name: &str) -> ObjectNamePart {
        ObjectNamePart::Function(ObjectNamePartFunction { name: ident(name), args: vec![] })
    }

    #[test]
    fn schema_and_table_errors_when_no_identifier_parts() {
        let name = ObjectName(vec![func_part("now")]);

        let err = match schema_and_table(&name) {
            Ok(v) => panic!("expected Err(DocError::InvalidObjectName), got Ok({v:?})"),
            Err(e) => e,
        };

        match err {
            DocError::InvalidObjectName { message, .. } => {
                assert_eq!(message, "ObjectName had no identifier parts");
            }
            other => panic!("unexpected error: {other:?}"),
        }
    }

    #[test]
    fn schema_and_table_single_identifier() {
        let name = ObjectName(vec![ObjectNamePart::Identifier(ident("users"))]);

        let (schema, table) = match schema_and_table(&name) {
            Ok(v) => v,
            Err(e) => panic!("unexpected error: {e:?}"),
        };

        assert_eq!(schema, None);
        assert_eq!(table, "users");
    }

    #[test]
    fn schema_and_table_schema_and_table_with_function_ignored()
    -> Result<(), Box<dyn std::error::Error>> {
        let name = ObjectName(vec![
            ObjectNamePart::Identifier(ident("catalog")),
            ObjectNamePart::Identifier(ident("public")),
            func_part("some_func"),
            ObjectNamePart::Identifier(ident("orders")),
        ]);

        let (schema, table) = schema_and_table(&name)?;
        assert_eq!(schema, Some("public".to_owned()));
        assert_eq!(table, "orders");
        Ok(())
    }

    struct FailOnNthWrite {
        fail_at: usize,
        writes: usize,
        sink: String,
    }

    impl FailOnNthWrite {
        fn new(fail_at: usize) -> Self {
            Self { fail_at, writes: 0, sink: String::new() }
        }
    }

    impl fmt::Write for FailOnNthWrite {
        fn write_str(&mut self, s: &str) -> fmt::Result {
            self.writes += 1;
            if self.writes == self.fail_at {
                return Err(fmt::Error);
            }
            self.sink.push_str(s);
            Ok(())
        }
    }

    fn run_fail_at<T: fmt::Display>(v: &T, fail_at: usize) -> Result<(), fmt::Error> {
        let mut w = FailOnNthWrite::new(fail_at);
        fmt::write(&mut w, format_args!("{v}"))
    }

    fn count_writes<T: fmt::Display>(v: &T) -> usize {
        let mut w = FailOnNthWrite { fail_at: usize::MAX, writes: 0, sink: String::new() };
        let _ = fmt::write(&mut w, format_args!("{v}"));
        w.writes
    }

    #[test]
    fn display_propagates_every_question_mark_path_for_column_and_table() {
        let col_with_doc = ColumnDoc::new("col_a".into(), Some("doc".into()));
        let col_without_doc = ColumnDoc::new("col_b".into(), None);

        let table = TableDoc::new(
            Some("public".into()),
            "users".into(),
            Some("table doc".into()),
            vec![col_with_doc.clone(), col_without_doc],
            None,
        );

        let col_writes = count_writes(&col_with_doc);
        let table_writes = count_writes(&table);

        for i in 1..=col_writes {
            assert!(
                run_fail_at(&col_with_doc, i).is_err(),
                "ColumnDoc should error when failing at write #{i} (total writes {col_writes})"
            );
        }

        for i in 1..=table_writes {
            assert!(
                run_fail_at(&table, i).is_err(),
                "TableDoc should error when failing at write #{i} (total writes {table_writes})"
            );
        }
    }

    #[test]
    fn column_doc_set_doc_updates_doc() {
        let mut col = ColumnDoc::new("id".to_owned(), None);
        assert_eq!(col.name(), "id");
        assert_eq!(col.doc(), None);
        col.set_doc("primary key");
        assert_eq!(col.doc(), Some("primary key"));
        let new_doc = String::from("primary key for users table");
        col.set_doc(new_doc);
        assert_eq!(col.doc(), Some("primary key for users table"));
    }

    #[test]
    fn table_doc_set_doc_updates_doc() {
        let mut table = TableDoc::new(None, "users".to_owned(), None, Vec::new(), None);
        assert_eq!(table.name(), "users");
        assert_eq!(table.schema(), None);
        assert_eq!(table.doc(), None);
        table.set_doc("users table docs");
        assert_eq!(table.doc(), Some("users table docs"));
        table.set_doc(String::from("updated users table docs"));
        assert_eq!(table.doc(), Some("updated users table docs"));
    }

    #[test]
    fn columns_mut_allows_mutating_column_docs() {
        let mut table = TableDoc::new(
            None,
            "users".to_owned(),
            None,
            vec![
                ColumnDoc::new("id".to_owned(), None),
                ColumnDoc::new("username".to_owned(), None),
            ],
            None,
        );

        {
            let cols_mut = table.columns_mut();
            assert_eq!(cols_mut.len(), 2);
            cols_mut[0].set_doc("primary key");
            cols_mut[1].set_doc("login name");
        }

        let cols = table.columns();
        assert_eq!(cols[0].name(), "id");
        assert_eq!(cols[0].doc(), Some("primary key"));
        assert_eq!(cols[1].name(), "username");
        assert_eq!(cols[1].doc(), Some("login name"));
    }
    #[test]
    fn test_from_sql_file_doc_into_vec_table_doc_preserves_contents_and_order() {
        let t1 = TableDoc::new(
            None,
            "users".to_owned(),
            Some("users doc".to_owned()),
            vec![ColumnDoc::new("id".to_owned(), Some("pk".to_owned()))],
            None,
        );
        let t2 = TableDoc::new(
            Some("analytics".to_owned()),
            "events".to_owned(),
            None,
            vec![ColumnDoc::new("payload".to_owned(), None)],
            None,
        );

        let sql_file_doc = SqlFileDoc::new(vec![t1.clone(), t2.clone()]);
        let got: Vec<TableDoc> = Vec::from(sql_file_doc);

        let expected = vec![t1, t2];
        assert_eq!(got, expected);
    }
    #[test]
    fn test_table_doc_path_getter_returns_expected_value() {
        let mut table = TableDoc::new(None, "users".to_owned(), None, Vec::new(), None);
        assert_eq!(table.path(), None);
        let pb = PathBuf::from("some/dir/file.sql");
        table.set_path(Some(pb.clone()));
        assert_eq!(table.path(), Some(pb.as_path()));
        let no_path: Option<PathBuf> = None;
        table.set_path(no_path);
        assert_eq!(table.path(), None);
    }

    #[test]
    fn test_table_doc_column() {
        let table = TableDoc::new(
            None,
            "users".to_owned(),
            None,
            vec![
                ColumnDoc::new("id".to_owned(), None),
                ColumnDoc::new("username".to_owned(), None),
            ],
            None,
        );

        let col = table.column("id").unwrap_or_else(|_| panic!("Column 'id' should exist"));
        assert_eq!(col.name(), "id");

        let missing = table.column("nope");
        assert!(missing.is_err());
        assert!(matches!(missing, Err(DocError::ColumnNotFound { name }) if name == "nope"));
    }
}
