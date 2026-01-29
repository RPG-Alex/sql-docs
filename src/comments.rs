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

/// Enum for holding the comment content, differentiated by single line `--` and
/// multiline `/* */`
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CommentKind {
    /// Enum variant for Multiline Comments
    MultiLine(String),
    /// Enum variant for Single Line Comments
    SingleLine(String),
}

impl CommentKind {
    /// Getter for returning the comment from within the enum
    #[must_use]
    pub fn comment(&self) -> &str {
        match self {
            Self::MultiLine(comment) | Self::SingleLine(comment) => comment,
        }
    }
    /// Setter method for setting the comment value
    pub fn set_comment(&mut self, text: &str) {
        match self {
            Self::MultiLine(s) | Self::SingleLine(s) => {
                *s = text.to_owned();
            }
        }
    }
}

/// Structure for containing the [`CommentKind`] and the [`Span`] for a comment
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Comment {
    kind: CommentKind,
    span: Span,
}

impl Comment {
    /// Method for making a new comment
    ///
    /// # Parameters
    /// - `kind` where the type of comment is passed as a [`CommentKind`]
    /// - `span` where the [`Span`] of the comment is passed
    #[must_use]
    pub const fn new(kind: CommentKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Getter method to get the [`CommentKind`]
    #[must_use]
    pub const fn kind(&self) -> &CommentKind {
        &self.kind
    }

    /// Getter method to get the [`Span`] of the comment
    #[must_use]
    pub const fn span(&self) -> &Span {
        &self.span
    }

    /// Getter method that will return the comment content as a [`str`],
    /// regardless of [`CommentKind`]
    #[must_use]
    pub fn text(&self) -> &str {
        self.kind().comment()
    }

    /// Set the [`CommentKind`] for the comment
    pub fn set_kind(&mut self, kind: CommentKind) -> &mut Self {
        self.kind = kind;
        self
    }

    /// Sets new [`Span`] locations for the comment
    pub const fn set_span_locations(&mut self, start: Location, end: Location) -> &mut Self {
        self.span = Span::new(start, end);
        self
    }
}

impl Default for Comment {
    fn default() -> Self {
        Self { kind: CommentKind::SingleLine(String::new()), span: Span::default() }
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
        let mut col;

        let mut in_single = false;
        let mut in_multi = false;

        let mut buf = String::new();

        for line in src.lines() {
            col = 1;
            let mut chars = line.chars().peekable();
            while let Some(c) = chars.next() {
                match (in_single, in_multi, c) {
                    (false, false, '-') => {
                        if chars.peek().copied() == Some('-') {
                            chars.next();
                            in_single = true;
                            start_line = line_num;
                            start_col = col;
                            buf.clear();
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
                            let loc = Location::new(line_num, col);
                            return Err(CommentError::UnmatchedMultilineCommentStart {
                                location: loc,
                            });
                        }
                    }
                    (false, true, '*') => {
                        if chars.peek().copied() == Some('/') {
                            chars.next();
                            let end_loc = Location::new(line_num, col + 1);
                            let normalized_comment = buf
                                .lines()
                                .enumerate()
                                .map(|(i, line)| match i {
                                    0 => line.trim().to_owned(),
                                    _ => "\n".to_owned() + line.trim(),
                                })
                                .collect();
                            comments.push(Comment::new(
                                CommentKind::MultiLine(normalized_comment),
                                Span::new(
                                    Location { line: start_line, column: start_col },
                                    end_loc,
                                ),
                            ));
                            in_multi = false;
                            buf.clear();
                            col += 1;
                        } else {
                            buf.push('*');
                        }
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
                let end_loc = Location::new(line_num, col);
                comments.push(Comment::new(
                    CommentKind::SingleLine(buf.trim().to_owned()),
                    Span::new(Location { line: start_line, column: start_col }, end_loc),
                ));
                buf.clear();
            } else if in_multi {
                buf.push('\n');
            }
            line_num += 1;
        }
        // EOF: close any open comments
        if in_multi {
            return Err(CommentError::UnterminatedMultiLineComment {
                start: Location { line: start_line, column: start_col },
            });
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

    /// Method for retrieving all comments that occur directly before a specified line
    ///
    /// # Parameters
    /// - `self` the current [`Comments`] object
    /// - The `line` as a [`u64`] to use for reference
    pub fn all_valid_leading_comments(&self, line: u64) -> Vec<&Comment> {
        let mut found_comments = Vec::new();
        let mut current_line = match line.checked_sub(1) {
            Some(n) => n,
            None => return found_comments,
        };
        for comment in self.comments().iter().rev() {
            let start = comment.span().end().line();
            let end = comment.span().end().line();
            if end == current_line {
                found_comments.push(comment);
                if current_line == 0 {
                    break;
                }
                current_line = start.saturating_sub(1);
            } else if comment.span().end().line() < line {
                break;
            }
        }
        found_comments.reverse();
        found_comments
    }
}

fn combine_leading_comments(comments: Vec<&Comment>) -> Comment {
    let mut final_comment: Comment = Comment::default();
    let kind = comments[0].kind();
    final_comment.set_kind(kind.clone());
    let mut content = String::new();
    let start = comments[0].span().start();
    let mut end = Location::default();
    for comment in comments.iter().rev() {
        end = *comment.span().end();
        content.push_str(comment.text());
    }

    final_comment.kind.set_comment(&content);
    final_comment.set_span_locations(start.to_owned(), end);

    final_comment
}

#[cfg(test)]
mod tests {
    use std::{env, fs};

    use crate::comments::{Comment, CommentError, CommentKind, Comments, Location, Span};

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

        let singleline = CommentKind::SingleLine(raw_comment.to_owned());
        let mut span = Span::default();
        span.end.column = len - 1;

        let comment = Comment::new(singleline.clone(), span);

        assert_eq!(comment.kind, singleline);

        let expected_span =
            Span::new(Location { line: 1, column: 1 }, Location { line: 1, column: len - 1 });

        assert_eq!(comment.span, expected_span);
    }

    #[test]
    fn multiline_comment_span() {
        let kind = CommentKind::MultiLine("/* hello world */".to_owned());
        let span = Span::new(Location { line: 1, column: 1 }, Location { line: 2, column: 9 });

        let comment = Comment::new(kind.clone(), span);

        assert_eq!(comment.kind, kind);
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
            let parsed_comments = Comments::parse_all_comments_from_file(file)?;
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
            assert_eq!(expected[i], comment.kind().comment(), "comment at index {i} did not match");
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
        assert_eq!(first.kind().comment(), "Users table stores user account information");
        assert_eq!(first.span().start(), &Location::new(1, 1));
        assert_eq!(first.span().end(), &Location::new(1, 47));
        let primary_key = &comments[1];
        assert_eq!(primary_key.kind().comment(), "Primary key");
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
        assert_eq!(
            first.kind().comment(),
            "Users table stores user account information\nmultiline"
        );
        assert_eq!(first.span().start(), &Location::new(1, 1));
        assert_eq!(first.span().end().line(), 2);
        assert!(
            first.span().end().column() > first.span().start().column(),
            "end column should be after start column for first multiline comment",
        );
        let primary_key = &comments[1];
        assert_eq!(primary_key.kind().comment(), "Primary key\nmultiline");
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
                CommentKind::SingleLine("a comment".to_owned()),
                Span { start: Location::new(1, 1), end: Location::new(1, 12) },
            ),
            Comment::new(
                CommentKind::SingleLine("a second comment".to_owned()),
                Span { start: Location::new(1, 1), end: Location::new(2, 19) },
            ),
        ];
        let length = comment_vec.len();
        let comments = Comments::new(comment_vec.clone());
        assert!(comments.comments().len() == length);
        for (i, comment) in comments.comments().iter().enumerate() {
            assert_eq!(comment.kind().comment(), comment_vec[i].kind().comment());
            assert_eq!(comment.span().start(), comment_vec[i].span().start());
            assert_eq!(comment.span().end(), comment_vec[i].span().end());
        }
    }

    #[test]
    fn test_all_valid_leading_comments() {
        // Lines:
        // 1: -- old
        // 2:
        // 3: -- a
        // 4: -- b
        // 5: implied statement
        let comment_vec = vec![
            Comment::new(
                CommentKind::SingleLine("old".to_owned()),
                Span::new(Location::new(1, 1), Location::new(1, 6)),
            ),
            Comment::new(
                CommentKind::SingleLine("a".to_owned()),
                Span::new(Location::new(3, 1), Location::new(3, 4)),
            ),
            Comment::new(
                CommentKind::SingleLine("b".to_owned()),
                Span::new(Location::new(4, 1), Location::new(4, 4)),
            ),
        ];

        let comments = Comments::new(comment_vec);

        let leading = comments.all_valid_leading_comments(5);

        assert_eq!(leading.len(), 2);
        assert_eq!(leading[0].text(), "a");
        assert_eq!(leading[1].text(), "b");

        // Also confirm the spans align to those lines.
        assert_eq!(leading[0].span().start().line(), 3);
        assert_eq!(leading[1].span().end().line(), 4);
    }
}
