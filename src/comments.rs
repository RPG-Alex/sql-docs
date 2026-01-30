//! Extract comment spans from parsed SQL files.
//!
//! Definitions used throughout this crate:
//! - **leading**: a comment that appears on lines immediately preceding a statement/column
//! - **inline**: a comment that appears after code on the same line (ignored)
//! - **interstitial**: a comment inside a statement (ignored)

use std::fmt;

use crate::ast::ParsedSqlFile;

/// Represents a line/column location within a source file.
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
pub struct Location {
    line: u64,
    column: u64,
}

impl Location {
    /// Method for instantiating a new [`Location`]
    ///
    /// # Parameters
    /// - line: the [`u64`] value of the line location
    /// - column: the [`u64`] value of the column location
    #[must_use]
    pub const fn new(line: u64, column: u64) -> Self {
        Self { line, column }
    }

    /// Getter method for getting the line value
    #[must_use]
    pub const fn line(&self) -> u64 {
        self.line
    }

    /// Getter method for getting the column value
    #[must_use]
    pub const fn column(&self) -> u64 {
        self.column
    }
}

impl Default for Location {
    fn default() -> Self {
        Self::new(1, 1)
    }
}

/// Represents a start/end span (inclusive/exclusive as used by this crate) for a comment in a file.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Span {
    start: Location,
    end: Location,
}

impl Span {
    /// Method for creating a new instance of the [`Span`] for a
    /// comment's span
    ///
    /// # Parameters
    /// - the [`Location`] where the comment starts in the file
    /// - the [`Location`] where the comment ends in the file
    #[must_use]
    pub const fn new(start: Location, end: Location) -> Self {
        Self { start, end }
    }

    /// Getter for the start location of a [`Span`]
    #[must_use]
    pub const fn start(&self) -> &Location {
        &self.start
    }

    /// Getter for the end location of a [`Span`]
    #[must_use]
    pub const fn end(&self) -> &Location {
        &self.end
    }
}

impl Default for Span {
    fn default() -> Self {
        Self::new(Location::default(), Location::default())
    }
}

/// Structure for containing the comment, location and style
#[derive(Clone, Default, Debug, Eq, PartialEq)]
pub struct Comment {
    /// The comment content found as a [`String`]
    text: String,
    /// The location of the comment start/finish as a [`Span`]
    span: Span,
}

impl Comment {
    /// Method for making a new comment
    ///
    /// # Parameters
    /// - `text` the text of the comment as a [`String`]
    /// - `span` where the [`Span`] of the comment is passed
    #[must_use]
    pub const fn new(text: String, span: Span) -> Self {
        Self { text, span }
    }

    /// Getter method to get the [`Span`] of the comment
    #[must_use]
    pub const fn span(&self) -> &Span {
        &self.span
    }

    /// Getter method that will return the comment content as a &[`str`]
    #[must_use]
    pub fn text(&self) -> &str {
        &self.text
    }

    /// Setter for text of comment
    pub fn set_text(&mut self, text: impl Into<String>) {
        self.text = text.into();
    }

    /// Sets new [`Span`] locations for the comment
    pub const fn set_span_locations(&mut self, start: Location, end: Location) {
        self.span = Span::new(start, end);
    }
}

/// Enum for returning errors withe Comment parsing
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CommentError {
    /// Found a multiline comment terminator `*/` without a matching opener `/*`
    UnmatchedMultilineCommentStart {
        /// Returns the location of the terminator found
        location: Location,
    },
    /// Found a multiline comment that is not properly terminated before EOF
    UnterminatedMultiLineComment {
        /// Returns the location of where the multiline comment started
        start: Location,
    },
}

impl fmt::Display for CommentError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnmatchedMultilineCommentStart { location } => {
                write!(
                    f,
                    "unmatched block comment start at line {}, column {}",
                    location.line(),
                    location.column()
                )
            }
            Self::UnterminatedMultiLineComment { start } => {
                write!(
                    f,
                    "unterminated block comment with start at line {}, column {}",
                    start.line(),
                    start.column(),
                )
            }
        }
    }
}

impl std::error::Error for CommentError {}

/// Alias for comment results that may return a [`CommentError`]
pub type CommentResult<T> = Result<T, CommentError>;

/// Structure for holding all comments found in the document
#[derive(Debug, Eq, PartialEq)]
pub struct Comments {
    comments: Vec<Comment>,
}

impl Comments {
    /// Method for generating a new [`Comments`] struct, which sorts comments
    /// based on their starting span location
    ///
    /// # Parameters
    /// - `comments`: mutable [`Vec<Comment>`] that will be sorted by span start
    #[must_use]
    pub fn new(mut comments: Vec<Comment>) -> Self {
        // Always keep comments ordered by their span
        comments.sort_by(|a, b| {
            let a_start = a.span().start();
            let b_start = b.span().start();

            a_start
                .line()
                .cmp(&b_start.line())
                .then_with(|| a_start.column().cmp(&b_start.column()))
        });

        Self { comments }
    }

    /// Build all leading comments from a parsed SQL file
    ///
    /// # Parameters
    /// - `file`: the [`ParsedSqlFile`] that needs to be parsed for comments
    ///
    /// # Errors
    /// - Will return [`CommentError::UnmatchedMultilineCommentStart`] if a
    ///   comment does not have an opening `/*`
    /// - Will return [`CommentError::UnterminatedMultiLineComment`] if a
    ///   multiline comment doesn't end before `EOF`
    pub fn parse_all_comments_from_file(file: &ParsedSqlFile) -> CommentResult<Self> {
        let src = file.content();
        let comments = Self::scan_comments(src)?;
        Ok(comments)
    }

    /// Scans the raw file and collects all comments
    ///
    /// # Parameters
    /// - `src` which is the `SQL` file content as a [`str`]
    ///
    /// # Errors
    /// - `UnmatchedMultilineCommentStart` : will return error if unable to find
    ///   a starting `/*` for a multiline comment
    /// - `UnterminatedMultiLineComment` : will return error if there is an
    ///   unterminated multiline comment, found at EOF
    pub fn scan_comments(src: &str) -> CommentResult<Self> {
        let mut comments = Vec::new();

        let mut start_line = 1u64;
        let mut start_col = 1u64;

        let mut line_num = 1u64;

        let mut in_single = false;
        let mut in_multi = false;

        let lines: Vec<&str> = src.lines().collect();

        let mut buf = String::new();

        for (i, line) in lines.iter().enumerate() {
            let mut col = 1;
            let mut chars = line.chars().peekable();
            while let Some(c) = chars.next() {
                match (in_single, in_multi, c) {
                    (false, false, '-') => {
                        if chars.peek().copied() == Some('-') {
                            chars.next();
                            in_single = true;
                            let continuing = i
                                .checked_sub(1)
                                .and_then(|j| lines.get(j))
                                .is_some_and(|prev| prev.trim().starts_with("--"));
                            if !continuing {
                                buf.clear();
                                start_line = line_num;
                                start_col = col;
                            }
                            col += 1;
                        }
                    }
                    (false, false, '/') => {
                        if chars.peek().copied() == Some('*') {
                            chars.next();
                            in_multi = true;
                            start_line = line_num;
                            start_col = col;
                            buf.clear();
                            col += 1;
                        }
                    }
                    (false, false, '*') => {
                        if chars.peek().copied() == Some('/') {
                            return Err(CommentError::UnmatchedMultilineCommentStart {
                                location: Location::new(line_num, col),
                            });
                        }
                    }
                    (false, true, '*') => {
                        if chars.peek().copied() != Some('/') {
                            buf.push('*');
                            continue;
                        }
                        chars.next();
                        let normalized_comment =
                            buf.lines().map(str::trim).collect::<Vec<_>>().join("\n");

                        comments.push(Comment::new(
                            normalized_comment,
                            Span::new(
                                Location { line: start_line, column: start_col },
                                Location::new(line_num, col + 1),
                            ),
                        ));
                        in_multi = false;
                        buf.clear();
                        col += 1;
                    }
                    (false, true, ch) | (true, false, ch) => {
                        buf.push(ch);
                    }
                    (false, false, _) => {}
                    (true, true, _) => {
                        unreachable!("should not be possible to be in multiline and single line")
                    }
                }
                col += 1;
            }
            if in_single {
                in_single = false;
                if lines.get(i + 1).is_some_and(|line| line.trim().starts_with("--")) {
                    buf.push('\n');
                } else {
                    comments.push(Comment::new(
                        buf.trim().replace("\n ", "\n").to_owned(),
                        Span::new(
                            Location { line: start_line, column: start_col },
                            Location::new(line_num, col),
                        ),
                    ));
                    buf.clear();
                }
            } else if in_multi {
                buf.push('\n');
            }
            line_num += 1;
        }
        Ok(Self { comments })
    }

    /// Getter method for retrieving the Vec of [`Comment`]
    #[must_use]
    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }

    /// Helper method for checking and finding for a comment before a specific
    /// line
    ///
    /// # Parameters
    /// - `self` an instance of [`Comments`]
    /// - `line` an `u64` value representing the desired line to check above.
    #[must_use]
    pub fn leading_comment(&self, line: u64) -> Option<&Comment> {
        self.comments().iter().rev().find(|comment| comment.span().end().line() + 1 == line)
    }
}

#[cfg(test)]
mod tests {
    use std::{env, fs};

    use crate::comments::{Comment, CommentError, Comments, Location, Span};

    #[test]
    fn location_new_and_default() {
        let mut location = Location::new(2, 5);
        location.column = 20;
        location.line = 43;

        assert_eq!(Location { column: 20, line: 43 }, location);

        let location2 = Location::default();
        assert_eq!(location2, Location { line: 1, column: 1 });
    }

    #[test]
    fn span_default_and_updates() {
        let default = Span::default();
        assert_eq!(default.start, Location::default());
        assert_eq!(default.end, Location::default());

        let span = Span { end: Location::new(55, 100), ..Default::default() };

        assert_eq!(span.start, Location::default());
        assert_eq!(span.end, Location { line: 55, column: 100 });
    }

    #[test]
    fn comments_with_comment_kind() {
        let raw_comment = "-- a comment";
        let len = raw_comment.len() as u64;

        let singleline = raw_comment.to_owned();
        let mut span = Span::default();
        span.end.column = len - 1;

        let comment = Comment::new(singleline.clone(), span);

        assert_eq!(comment.text(), singleline);

        let expected_span =
            Span::new(Location { line: 1, column: 1 }, Location { line: 1, column: len - 1 });

        assert_eq!(comment.span, expected_span);
    }

    #[test]
    fn multiline_comment_span() {
        let text = "/* hello world */".to_owned();
        let span = Span::new(Location { line: 1, column: 1 }, Location { line: 2, column: 9 });

        let comment = Comment::new(text.clone(), span);

        assert_eq!(comment.text(), text);
        assert_eq!(comment.span.start.line, 1);
        assert_eq!(comment.span.end.line, 2);
    }

    #[test]
    fn parse_comments() -> Result<(), Box<dyn std::error::Error>> {
        use crate::{ast::ParsedSqlFileSet, comments::Comments, files::SqlFileSet};
        let base = env::temp_dir().join("all_sql_files");
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

        for file in parsed_set.files() {
            let parsed_comments = Comments::parse_all_comments_from_file(file).map_err(|e| {
                format!(
                    "parse_all_comments_from_file failed for file {:?}: {e}",
                    file.file().path()
                )
            })?;

            let filename = file
                .file()
                .path()
                .and_then(|p| p.file_name())
                .and_then(|s| s.to_str())
                .ok_or("Should have a file name")?;

            match filename {
                "with_single_line_comments.sql" => {
                    assert_parsed_comments_eq(&parsed_comments, expected_single_line_comments());
                }
                "with_multiline_comments.sql" => {
                    assert_parsed_comments_eq(&parsed_comments, expected_multiline_comments());
                }
                "with_mixed_comments.sql" => {
                    assert_parsed_comments_eq(&parsed_comments, expected_mixed_comments());
                }
                "without_comments.sql" => {
                    assert!(parsed_comments.comments().is_empty());
                }
                other => {
                    unreachable!(
                        "unexpected test file {other}; directory should only contain known test files"
                    );
                }
            }
        }
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }

    fn assert_parsed_comments_eq(parsed: &Comments, expected: &[&str]) {
        let comments = parsed.comments();
        assert_eq!(
            expected.len(),
            comments.len(),
            "mismatched comment count (expected {}, got {})",
            expected.len(),
            comments.len()
        );

        for (i, comment) in comments.iter().enumerate() {
            assert_eq!(expected[i], comment.text(), "comment at index {i} did not match");
        }
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

    fn expected_single_line_comments() -> &'static [&'static str] {
        &[
            "Users table stores user account information",
            "Primary key",
            "Username for login",
            "Email address",
            "When the user registered",
            "Posts table stores blog posts",
            "Primary key",
            "Post title",
            "Foreign key linking to users",
            "Main body text",
            "When the post was created",
        ]
    }

    fn expected_multiline_comments() -> &'static [&'static str] {
        &[
            "Users table stores user account information\nmultiline",
            "Primary key\nmultiline",
            "Username for login\nmultiline",
            "Email address\nmultiline",
            "When the user registered\nmultiline",
            "Posts table stores blog posts\nmultiline",
            "Primary key\nmultiline",
            "Post title\nmultiline",
            "Foreign key linking to users\nmultiline",
            "Main body text\nmultiline",
            "When the post was created\nmultiline",
        ]
    }

    fn expected_mixed_comments() -> &'static [&'static str] {
        &[
            "interstitial Comment above statements (should be ignored)",
            "Users table stores user account information",
            "users interstitial comment\n(should be ignored)",
            "Primary key",
            "Id comment that is interstitial (should be ignored)",
            "Username for login",
            "Email address",
            "When the user registered",
            "Posts table stores blog posts",
            "Primary key",
            "Post title",
            "Foreign key linking to users",
            "Main body text",
            "When the post was created",
        ]
    }

    #[test]
    fn single_line_comment_spans_are_correct() -> Result<(), Box<dyn std::error::Error>> {
        use crate::{ast::ParsedSqlFileSet, files::SqlFileSet};
        let base = env::temp_dir().join("single_line_spans");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let file = base.join("single.sql");
        fs::File::create(&file)?;
        fs::write(&file, single_line_comments_sql())?;
        let set = SqlFileSet::new(&base, &[])?;
        let parsed_set = ParsedSqlFileSet::parse_all(set)?;
        let file = parsed_set
            .files()
            .iter()
            .find(|f| {
                f.file().path().and_then(|p| p.to_str()).is_some_and(|p| p.ends_with("single.sql"))
            })
            .ok_or("single.sql should be present")?;

        let comments = Comments::parse_all_comments_from_file(file)?;
        let comments = comments.comments();
        assert_eq!(comments.len(), 11);
        let first = &comments[0];
        assert_eq!(first.text(), "Users table stores user account information");
        assert_eq!(first.span().start(), &Location::new(1, 1));
        assert_eq!(first.span().end(), &Location::new(1, 47));
        let primary_key = &comments[1];
        assert_eq!(primary_key.text(), "Primary key");
        assert_eq!(primary_key.span().start(), &Location::new(3, 5));
        assert_eq!(primary_key.span().end(), &Location::new(3, 19));
        assert!(
            primary_key.span().end().column() > primary_key.span().start().column(),
            "end column should be after start column",
        );
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }

    #[test]
    fn multiline_comment_spans_are_correct() -> Result<(), Box<dyn std::error::Error>> {
        use crate::{ast::ParsedSqlFileSet, files::SqlFileSet};
        let base = env::temp_dir().join("multi_line_spans");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let file = base.join("multi.sql");
        fs::File::create(&file)?;
        fs::write(&file, multiline_comments_sql())?;
        let set = SqlFileSet::new(&base, &[])?;
        let parsed_set = ParsedSqlFileSet::parse_all(set)?;
        let file = parsed_set
            .files()
            .iter()
            .find(|f| {
                f.file().path().and_then(|p| p.to_str()).is_some_and(|p| p.ends_with("multi.sql"))
            })
            .ok_or("multi.sql should be present")?;

        let comments = Comments::parse_all_comments_from_file(file)?;
        let comments = comments.comments();
        assert_eq!(comments.len(), 11);
        let first = &comments[0];
        assert_eq!(first.text(), "Users table stores user account information\nmultiline");
        assert_eq!(first.span().start(), &Location::new(1, 1));
        assert_eq!(first.span().end().line(), 2);
        assert!(
            first.span().end().column() > first.span().start().column(),
            "end column should be after start column for first multiline comment",
        );
        let primary_key = &comments[1];
        assert_eq!(primary_key.text(), "Primary key\nmultiline");
        assert_eq!(primary_key.span().start(), &Location::new(4, 5));
        assert_eq!(primary_key.span().end().line(), 5);
        assert!(
            primary_key.span().end().column() > primary_key.span().start().column(),
            "end column should be after start column for primary key multiline comment",
        );
        let _ = fs::remove_dir_all(&base);
        Ok(())
    }

    #[test]
    fn test_comment_error() {
        let unterminated =
            CommentError::UnterminatedMultiLineComment { start: Location::default() };
        let location = Location { line: 1, column: 1 };
        let expected = format!(
            "unterminated block comment with start at line {}, column {}",
            location.line(),
            location.column()
        );
        assert_eq!(unterminated.to_string(), expected);
    }

    #[test]
    fn test_comments() {
        let comment_vec = vec![
            Comment::new(
                "a comment".to_owned(),
                Span { start: Location::new(1, 1), end: Location::new(1, 12) },
            ),
            Comment::new(
                "a second comment".to_owned(),
                Span { start: Location::new(1, 1), end: Location::new(2, 19) },
            ),
        ];
        let length = comment_vec.len();
        let comments = Comments::new(comment_vec.clone());
        assert!(comments.comments().len() == length);
        for (i, comment) in comments.comments().iter().enumerate() {
            assert_eq!(comment.text(), comment_vec[i].text());
            assert_eq!(comment.span().start(), comment_vec[i].span().start());
            assert_eq!(comment.span().end(), comment_vec[i].span().end());
        }
    }

    #[test]
    fn single_line_runover_comments_are_combined() -> Result<(), Box<dyn std::error::Error>> {
        use crate::{ast::ParsedSqlFileSet, comments::Comments, files::SqlFileSet};
        use std::{env, fs};

        let base = env::temp_dir().join("single_line_runover_combined");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;

        let file = base.join("runover.sql");
        fs::File::create(&file)?;

        let sql = "\
-- comment 1
-- comment 2
SELECT 1;
";
        fs::write(&file, sql)?;

        let set = SqlFileSet::new(&base, &[])?;
        let parsed_set = ParsedSqlFileSet::parse_all(set)?;

        let parsed_file = parsed_set
            .files()
            .iter()
            .find(|f| {
                f.file().path().and_then(|p| p.to_str()).is_some_and(|p| p.ends_with("runover.sql"))
            })
            .ok_or("runover.sql should be present")?;

        let comments = Comments::parse_all_comments_from_file(parsed_file)?;
        let comments = comments.comments();

        assert_eq!(comments.len(), 1, "expected a single combined comment");
        assert_eq!(comments[0].text(), "comment 1\ncomment 2");

        assert_eq!(comments[0].span().start(), &Location::new(1, 1));
        assert_eq!(comments[0].span().end().line(), 2);

        let _ = fs::remove_dir_all(&base);
        Ok(())
    }

    #[test]
    fn single_line_comments_with_gap_are_not_combined() -> Result<(), Box<dyn std::error::Error>> {
        use crate::{ast::ParsedSqlFileSet, comments::Comments, files::SqlFileSet};
        use std::{env, fs};

        let base = env::temp_dir().join("single_line_runover_not_combined");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;

        let file = base.join("gap.sql");
        fs::File::create(&file)?;

        let sql = "\
-- comment 1

-- comment 2
SELECT 1;
";
        fs::write(&file, sql)?;

        let set = SqlFileSet::new(&base, &[])?;
        let parsed_set = ParsedSqlFileSet::parse_all(set)?;

        let parsed_file = parsed_set
            .files()
            .iter()
            .find(|f| {
                f.file().path().and_then(|p| p.to_str()).is_some_and(|p| p.ends_with("gap.sql"))
            })
            .ok_or("gap.sql should be present")?;

        let comments = Comments::parse_all_comments_from_file(parsed_file)?;
        let comments = comments.comments();

        assert_eq!(comments.len(), 2, "expected two separate comments");
        assert_eq!(comments[0].text(), "comment 1");
        assert_eq!(comments[1].text(), "comment 2");

        assert_eq!(comments[0].span().start(), &Location::new(1, 1));
        assert_eq!(comments[0].span().end().line(), 1);
        assert_eq!(comments[1].span().start(), &Location::new(3, 1));

        let _ = fs::remove_dir_all(&base);
        Ok(())
    }
}
