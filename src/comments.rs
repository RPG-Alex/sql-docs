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
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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

/// Enum for differentiating comments by single line `--` and
/// multiline `/* */`
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CommentKind {
    /// Enum variant for Multiline Comments
    MultiLine,
    /// Enum variant for Single Line Comments
    SingleLine,
}

/// Structure for containing the [`CommentKind`] and the [`Span`] for a comment
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Comment {
    text: String,
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
    pub const fn new(text: String, kind: CommentKind, span: Span) -> Self {
        Self { text, kind, span }
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
        &self.text
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
                                normalized_comment,
                                CommentKind::MultiLine,
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
                    buf.trim().to_owned(),
                    CommentKind::SingleLine,
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

    /// Finds a single comment before a specific line or returns none
    ///
    /// # Parameters
    /// - [`Comments`] object
    /// - An `u64` value representing the desired line to check above.
    #[must_use]
    pub fn leading_comment(&self, line: u64) -> Option<&Comment> {
        self.comments().iter().rev().find(|comment| comment.span().end().line() + 1 == line)
    }

    /// Finds leading comments before specific line based on [`LeadingCommentCapture`] preference
    ///
    /// # Parameters
    /// - [`Comments`] object
    /// - An `u64` value representing the desired line to check above.
    /// - [`LeadingCommentCapture`] preference
    #[must_use]
    pub fn leading_comments(&self, line: u64, capture: LeadingCommentCapture) -> Self {
        let mut comments: Vec<Comment> = Vec::new();
        let mut current_line = line;
        let mut seen_multiline = false;
        while let Some(leading_comment) = self.leading_comment(current_line) {
            match capture {
                LeadingCommentCapture::SingleNearest => {
                    comments.push(leading_comment.to_owned());
                    break;
                }
                LeadingCommentCapture::AllLeading => comments.push(leading_comment.to_owned()),
                LeadingCommentCapture::AllSingleOneMulti => match leading_comment.kind() {
                    CommentKind::MultiLine if seen_multiline => break,
                    CommentKind::MultiLine => {
                        seen_multiline = true;
                        comments.push(leading_comment.to_owned());
                    }
                    CommentKind::SingleLine => comments.push(leading_comment.to_owned()),
                },
            }
            current_line = leading_comment.span().start().line();
        }
        comments.reverse();
        Self::new(comments)
    }

    /// Collapse this collection of comments and separate each comment with `\n` as a single [`Comment`].
    #[must_use]
    pub fn collapse_comments(self) -> Option<Comment> {
        let mut iter = self.comments.into_iter();
        let first = iter.next()?;

        let Some(second) = iter.next() else {
            return Some(first);
        };

        let start = *first.span().start();

        let mut text = first.text().to_owned();
        text.push('\n');
        text.push_str(second.text());

        let mut end = *second.span().end();

        for c in iter {
            text.push('\n');
            text.push_str(c.text());
            end = *c.span().end();
        }

        Some(Comment::new(text, CommentKind::MultiLine, Span::new(start, end)))
    }
}

/// Controls how leading comments are captured for a statement.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Default)]
pub enum LeadingCommentCapture {
    /// Capture only the single nearest leading comment.
    #[default]
    SingleNearest,
    /// Capture all contiguous leading comments, stopping at the first blank line.
    AllLeading,
    /// Capture all contiguous single-line or at most one multi-line leading comments.
    AllSingleOneMulti,
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

        let singleline = CommentKind::SingleLine;
        let mut span = Span::default();
        span.end.column = len - 1;

        let comment = Comment::new(raw_comment.to_owned(), singleline.clone(), span);

        assert_eq!(comment.kind, singleline);

        let expected_span =
            Span::new(Location { line: 1, column: 1 }, Location { line: 1, column: len - 1 });

        assert_eq!(comment.span, expected_span);
    }

    #[test]
    fn multiline_comment_span() {
        let kind = CommentKind::MultiLine;
        let span = Span::new(Location { line: 1, column: 1 }, Location { line: 2, column: 9 });

        let comment = Comment::new("/* hello world */".to_owned(), kind.clone(), span);

        assert_eq!(comment.kind, kind);
        assert_eq!(comment.span.start.line, 1);
        assert_eq!(comment.span.end.line, 2);
    }

    #[test]
    fn parse_comments() -> Result<(), Box<dyn std::error::Error>> {
        use crate::{ast::ParsedSqlFileSet, comments::Comments, source::SqlSource};
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
        let set = SqlSource::sql_sources(&base, &[])?;
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
        use crate::{ast::ParsedSqlFileSet, source::SqlSource};
        let base = env::temp_dir().join("single_line_spans");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let file = base.join("single.sql");
        fs::File::create(&file)?;
        fs::write(&file, single_line_comments_sql())?;
        let set = SqlSource::sql_sources(&base, &[])?;
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
        use crate::{ast::ParsedSqlFileSet, source::SqlSource};
        let base = env::temp_dir().join("multi_line_spans");
        let _ = fs::remove_dir_all(&base);
        fs::create_dir_all(&base)?;
        let file = base.join("multi.sql");
        fs::File::create(&file)?;
        fs::write(&file, multiline_comments_sql())?;
        let set = SqlSource::sql_sources(&base, &[])?;
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
                CommentKind::SingleLine,
                Span { start: Location::new(1, 1), end: Location::new(1, 12) },
            ),
            Comment::new(
                "a second comment".to_owned(),
                CommentKind::SingleLine,
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

    use crate::comments::LeadingCommentCapture;

    fn texts(v: &Comments) -> Vec<String> {
        v.comments().iter().map(|c| c.text().to_owned()).collect()
    }

    #[test]
    fn leading_comment_capture_default_is_single_nearest() {
        match LeadingCommentCapture::default() {
            LeadingCommentCapture::SingleNearest => {}
            _ => panic!("Default for LeadingCommentCapture must be SingleNearest"),
        }
    }

    #[test]
    fn leading_comments_single_nearest_and_all_leading_basic_runover()
    -> Result<(), Box<dyn std::error::Error>> {
        let src = "\
-- c1
-- c2
CREATE TABLE t (id INTEGER);
";
        let parsed = Comments::scan_comments(src)?;
        let single = parsed.leading_comments(3, LeadingCommentCapture::SingleNearest);
        assert_eq!(texts(&single), vec!["c2".to_owned()]);

        let all = parsed.leading_comments(3, LeadingCommentCapture::AllLeading);
        assert_eq!(texts(&all), vec!["c1".to_owned(), "c2".to_owned()]);

        Ok(())
    }

    #[test]
    fn leading_comments_all_leading_stops_at_blank_line() -> Result<(), Box<dyn std::error::Error>>
    {
        let src = "\
-- c1

-- c2
CREATE TABLE t (id INTEGER);
";
        let parsed = Comments::scan_comments(src)?;
        let all = parsed.leading_comments(4, LeadingCommentCapture::AllLeading);
        assert_eq!(texts(&all), vec!["c2".to_owned()]);

        Ok(())
    }

    #[test]
    fn leading_comments_all_single_one_multi_collects_singles_and_one_multiline()
    -> Result<(), Box<dyn std::error::Error>> {
        let src = "\
/* m
m */
-- s1
-- s2
CREATE TABLE t (id INTEGER);
";
        let parsed = Comments::scan_comments(src)?;
        let got = parsed.leading_comments(5, LeadingCommentCapture::AllSingleOneMulti);
        assert_eq!(texts(&got), vec!["m\nm".to_owned(), "s1".to_owned(), "s2".to_owned(),]);

        Ok(())
    }

    #[test]
    fn leading_comments_all_single_one_multi_stops_before_second_multiline()
    -> Result<(), Box<dyn std::error::Error>> {
        let src = "\
/* m1 */
/* m2 */
-- s1
CREATE TABLE t (id INTEGER);
";
        let parsed = Comments::scan_comments(src)?;
        let got = parsed.leading_comments(4, LeadingCommentCapture::AllSingleOneMulti);
        assert_eq!(texts(&got), vec!["m2".to_owned(), "s1".to_owned()]);

        Ok(())
    }

    #[test]
    fn leading_comments_single_nearest_can_return_multiline()
    -> Result<(), Box<dyn std::error::Error>> {
        let src = "\
/* hello
world */
CREATE TABLE t (id INTEGER);
";
        let parsed = Comments::scan_comments(src)?;
        let got = parsed.leading_comments(3, LeadingCommentCapture::SingleNearest);
        assert_eq!(texts(&got), vec!["hello\nworld".to_owned()]);

        Ok(())
    }

    #[test]
    fn collapse_comments_empty_returns_none() {
        let comments = Comments::new(vec![]);
        assert!(comments.collapse_comments().is_none());
    }

    #[test]
    fn collapse_comments_single_returns_same_comment() {
        let c = Comment::new(
            "solo".to_owned(),
            CommentKind::SingleLine,
            Span::new(Location::new(10, 3), Location::new(10, 11)),
        );
        let comments = Comments::new(vec![c]);

        let collapsed =
            comments.collapse_comments().unwrap_or_else(|| panic!("should return a comment"));
        assert_eq!(collapsed.text(), "solo");
        assert_eq!(collapsed.kind(), &CommentKind::SingleLine);
        assert_eq!(collapsed.span(), &Span::new(Location::new(10, 3), Location::new(10, 11)));
    }

    #[test]
    fn collapse_comments_multiple_joins_text_and_expands_span_and_sets_multiline_kind() {
        let c1 = Comment::new(
            "a".to_owned(),
            CommentKind::SingleLine,
            Span::new(Location::new(1, 1), Location::new(1, 6)),
        );
        let c2 = Comment::new(
            "b".to_owned(),
            CommentKind::SingleLine,
            Span::new(Location::new(2, 1), Location::new(2, 6)),
        );
        let c3 = Comment::new(
            "c".to_owned(),
            CommentKind::MultiLine,
            Span::new(Location::new(3, 1), Location::new(4, 3)),
        );

        let comments = Comments::new(vec![c1, c2, c3]);

        let collapsed = comments.collapse_comments().unwrap_or_else(|| panic!("should collapse"));
        assert_eq!(collapsed.text(), "a\nb\nc");
        assert_eq!(collapsed.kind(), &CommentKind::MultiLine);
        assert_eq!(collapsed.span(), &Span::new(Location::new(1, 1), Location::new(4, 3)));
    }

    #[test]
    fn collapse_comments_with_leading_comments_allleading_collapses_correctly()
    -> Result<(), Box<dyn std::error::Error>> {
        let src = "\
-- c1
-- c2
CREATE TABLE t (id INTEGER);
";
        let parsed = Comments::scan_comments(src)?;

        let leading = parsed.leading_comments(3, LeadingCommentCapture::AllLeading);
        assert_eq!(texts(&leading), vec!["c1".to_owned(), "c2".to_owned()]);

        let collapsed = leading.collapse_comments().unwrap_or_else(|| panic!("should collapse"));
        assert_eq!(collapsed.text(), "c1\nc2");
        assert_eq!(collapsed.kind(), &CommentKind::MultiLine);

        // Span sanity: starts at first comment start, ends at second comment end.
        assert_eq!(*collapsed.span().start(), Location::new(1, 1));
        assert_eq!(collapsed.span().end().line(), 2);

        Ok(())
    }

    #[test]
    fn collapse_comments_with_leading_comments_single_nearest_preserves_kind()
    -> Result<(), Box<dyn std::error::Error>> {
        let src = "\
-- c1
-- c2
CREATE TABLE t (id INTEGER);
";
        let parsed = Comments::scan_comments(src)?;
        let leading = parsed.leading_comments(3, LeadingCommentCapture::SingleNearest);
        assert_eq!(texts(&leading), vec!["c2".to_owned()]);

        let collapsed = leading.collapse_comments().unwrap_or_else(|| panic!("should collapse"));
        assert_eq!(collapsed.text(), "c2");
        assert_eq!(collapsed.kind(), &CommentKind::SingleLine);

        Ok(())
    }
}
