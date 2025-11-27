//! Module for crawling the SQL documents based on the parser and
//! parsing/extracting the leading comments.
//!
//! *leading comment* a comment that
//! precedes an SQL Statement.
use std::fmt;

use crate::ast::ParsedSqlFile;

/// Structure for holding a location in the file. Assumes file is first split by
/// lines and then split by characters (column)
#[derive(Debug, Eq, PartialEq, PartialOrd)]
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

/// A structure for holding the span of comments found
#[derive(Debug, Eq, PartialEq)]
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
}

/// Structure for containing the [`CommentKind`] and the [`Span`] for a comment
#[derive(Debug, Eq, PartialEq)]
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
}

/// Enum for returning errors withe Comment parsing
#[derive(Debug)]
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

/// Structure that holds the comment along with its location in the file
pub struct CommentWithSpan {
    comment: Comment,
    span: Span,
}

impl CommentWithSpan {
    /// Method for creating a new [`CommentWithSpan`] from the comment
    /// [`String`] and the [`Span`]
    ///
    /// # Parameters
    /// - the comment as a [`String`]
    /// - the span of the comment as a [`Span`]
    #[must_use]
    pub const fn new(comment: Comment, span: Span) -> Self {
        Self { comment, span }
    }

    /// Getter method for retrieving the comment content
    #[must_use]
    pub const fn comment(&self) -> &Comment {
        &self.comment
    }

    /// Getter method for retrieving the [`Span`] of the comment
    #[must_use]
    pub const fn span(&self) -> &Span {
        &self.span
    }
}

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
    /// - Will return [`CommentError::UnmatchedMultilineCommentStart`] if a comment
    ///   does not have an opening `/*`
    /// - Will return [`CommentError::UnterminatedMultiLineComment`] if a multiline comment doesn't end before `EOF`
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

        let mut line = 1u64;
        let mut col = 1u64;

        let mut in_single = false;
        let mut in_multi = false;

        let mut buf = String::new();

        let mut chars = src.chars().peekable();

        while let Some(c) = chars.next() {
            match (in_single, in_multi, c) {
                (false, false, '-') => {
                    if chars.peek().copied() == Some('-') {
                        chars.next();
                        in_single = true;
                        start_line = line;
                        start_col = col;
                        buf.clear();
                        col += 1;
                    }
                }
                (false, false, '/') => {
                    if chars.peek().copied() == Some('*') {
                        chars.next();
                        in_multi = true;
                        start_line = line;
                        start_col = col;
                        buf.clear();
                        col += 1;
                    }
                }
                (false, false, '*') => {
                    if chars.peek().copied() == Some('*') {
                        let loc = Location::new(line, col);
                        return Err(CommentError::UnmatchedMultilineCommentStart { location: loc });
                    }
                }
                (true, false, '\n') => {
                    let end_loc = Location::new(line, col);
                    comments.push(Comment::new(
                        CommentKind::SingleLine(buf.trim().to_string()),
                        Span::new(Location { line: start_line, column: start_col }, end_loc),
                    ));
                    in_single = false;
                    buf.clear();
                }
                (false, true, '*') => {
                    if chars.peek().copied() == Some('/') {
                        chars.next();
                        let end_loc = Location::new(line, col + 1);
                        comments.push(Comment::new(
                            CommentKind::MultiLine(buf.trim().to_string()),
                            Span::new(Location { line: start_line, column: start_col }, end_loc),
                        ));
                        in_multi = false;
                        buf.clear();
                        col += 1;
                    } else {
                        buf.push('*');
                    }
                }
                (false, true, '\n') => {
                    buf.push('\n');
                }
                (false, true, ch) | (true, false, ch) => {
                    buf.push(ch);
                }
                (false, false, _) => {}
                (true, true, _) => {
                    unreachable!("should not be possible to be in multiline and single line")
                }
            }
            if c == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }

        // EOF: close any open comments
        if in_multi {
            return Err(CommentError::UnterminatedMultiLineComment {
                start: Location { line: start_line, column: start_col },
            });
        }

        if in_single && !buf.is_empty() {
            let end_loc = Location::new(line, col);
            comments.push(Comment::new(
                CommentKind::SingleLine(buf.trim_end().to_string()),
                Span::new(Location { line: start_line, column: start_col }, end_loc),
            ));
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

    let mut span = Span::default();
    span.end = Location::new(55, 100);

    assert_eq!(span.start, Location::default());
    assert_eq!(span.end, Location { line: 55, column: 100 });
}

#[test]
fn comments_with_comment_kind() {
    let raw_comment = "-- a comment";
    let len = raw_comment.len() as u64;

    let singleline = CommentKind::SingleLine(raw_comment.to_string());
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
    let kind = CommentKind::MultiLine("/* hello\nworld */".to_string());
    let span = Span::new(Location { line: 1, column: 1 }, Location { line: 2, column: 9 });

    let comment = Comment::new(kind.clone(), span);

    assert_eq!(comment.kind, kind);
    assert_eq!(comment.span.start.line, 1);
    assert_eq!(comment.span.end.line, 2);
}

#[test]
fn parse_comments() {
    use std::path::Path;

    use crate::{ast::ParsedSqlFileSet, files::SqlFileSet};
    let path = Path::new("sql_files");
    let set = SqlFileSet::new(path, None).unwrap();
    let parsed_set = ParsedSqlFileSet::parse_all(set).unwrap();
    for file in parsed_set.files().iter() {
        let parsed_comments = Comments::parse_all_comments_from_file(file).unwrap();
        match file.file().path().to_str().unwrap() {
            "sql_files/with_single_line_comments.sql" => {
                let expected = [
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
                ];
                assert_eq!(expected.len(), parsed_comments.comments().len());
                parsed_comments
                    .comments()
                    .iter()
                    .enumerate()
                    .for_each(|(i, c)| assert_eq!(expected[i], c.kind().comment()));
            }
            "sql_files/with_multiline_comments.sql" => {
                let expected = [
                    "Users table stores user account information \nmultiline",
                    "Primary key \n    multiline",
                    "Username for login \n    multiline",
                    "Email address \n    multiline",
                    "When the user registered \n    multiline",
                    "Posts table stores blog posts \nmultiline",
                    "Primary key \n    multiline",
                    "Post title \n    multiline",
                    "Foreign key linking to users \n    multiline",
                    "Main body text \n    multiline",
                    "When the post was created \n    multiline",
                ];
                assert_eq!(expected.len(), parsed_comments.comments().len());
                parsed_comments
                    .comments()
                    .iter()
                    .enumerate()
                    .for_each(|(i, c)| assert_eq!(expected[i], c.kind().comment()));
            }
            "sql_files/with_mixed_comments.sql" => {
                let expected = [
                    "interstitial Comment above statements (should be ignored)",
                    "Users table stores user account information",
                    "users interstitial comment \n(should be ignored)",
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
                ];
                assert_eq!(expected.len(), parsed_comments.comments().len());
                parsed_comments
                    .comments()
                    .iter()
                    .enumerate()
                    .for_each(|(i, c)| assert_eq!(expected[i], c.kind().comment()));
            }
            "sql_files/without_comments.sql" => {
                assert_eq!(0, parsed_comments.comments.len());
            }
            _ => {
                unreachable!(
                    "This shouldn't be accessible if directory parsed correctly and all test \
                     files accounted for"
                )
            }
        }
    }
}

#[test]
fn single_line_comment_spans_are_correct() {
    use std::path::Path;

    use crate::{ast::ParsedSqlFileSet, files::SqlFileSet};
    let path = Path::new("sql_files");
    let set = SqlFileSet::new(path, None).unwrap();
    let parsed_set = ParsedSqlFileSet::parse_all(set).unwrap();
    let file = parsed_set
        .files()
        .iter()
        .find(|f| f.file().path().to_str().unwrap().ends_with("with_single_line_comments.sql"))
        .expect("with_single_line_comments.sql should be present");
    let comments = Comments::parse_all_comments_from_file(file).unwrap();
    let comments = comments.comments();
    assert_eq!(comments.len(), 11);
    let first = &comments[0];
    assert_eq!(first.kind().comment(), "Users table stores user account information");
    assert_eq!(first.span().start(), &Location::new(1, 1));
    assert_eq!(first.span().end(), &Location::new(1, 47));
    let primary_key = &comments[1];
    assert_eq!(primary_key.kind().comment(), "Primary key");
    assert_eq!(primary_key.span().start(), &Location::new(3, 4));
    assert_eq!(primary_key.span().end(), &Location::new(3, 18));
    assert!(
        primary_key.span().end().column() > primary_key.span().start().column(),
        "end column should be after start column",
    );
}

#[test]
fn multiline_comment_spans_are_correct() {
    use std::path::Path;

    use crate::{ast::ParsedSqlFileSet, files::SqlFileSet};
    let path = Path::new("sql_files");
    let set = SqlFileSet::new(path, None).unwrap();
    let parsed_set = ParsedSqlFileSet::parse_all(set).unwrap();
    let file = parsed_set
        .files()
        .iter()
        .find(|f| f.file().path().to_str().unwrap().ends_with("with_multiline_comments.sql"))
        .expect("with_multiline_comments.sql should be present");
    let comments = Comments::parse_all_comments_from_file(file).unwrap();
    let comments = comments.comments();
    assert_eq!(comments.len(), 11);
    let first = &comments[0];
    assert_eq!(first.kind().comment(), "Users table stores user account information \nmultiline");
    assert_eq!(first.span().start(), &Location::new(1, 1));
    assert_eq!(first.span().end().line(), 2);
    assert!(
        first.span().end().column() > first.span().start().column(),
        "end column should be after start column for first multiline comment",
    );
    let primary_key = &comments[1];
    assert_eq!(primary_key.kind().comment(), "Primary key \n    multiline");
    assert_eq!(primary_key.span().start(), &Location::new(4, 4));
    assert_eq!(primary_key.span().end().line(), 5);
    assert!(
        primary_key.span().end().column() > primary_key.span().start().column(),
        "end column should be after start column for primary key multiline comment",
    );
}
