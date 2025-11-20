//! Module for crawling the SQL documents based on the parser and
//! parsing/extracting the leading comments.
//!
//! *leading comment* a comment that
//! precedes an SQL Statement.
use std::fmt;

use crate::ast::ParsedSqlFile;

/// Structure for holding a location in the file. Assumes file is first split by
/// lines and then split by characters (column)
#[derive(Debug)]
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
        Self::new(0, 0)
    }
}

/// A structure for holding the span of comments found
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

/// Enum for holding the comment content, differentiated by single line `--` and
/// multiline `/* */`
pub enum CommentKind {
    /// Enum variant for Multiline Comments
    MultiLine(String),
    /// Enum variant for Single Line Comments
    SingleLine(String),
}

/// Structure for containing the [`CommentKind`] and the [`Span`] for a comment
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
    pub const fn text(&self) -> &str {
        match &self.kind {
            CommentKind::SingleLine(s) | CommentKind::MultiLine(s) => s.as_str(),
        }
    }
}

/// Enum for returning errors withe Comment parsing
#[derive(Debug)]
pub enum CommentError {
    /// Found a block terminator `*/` without a matching opener `/*`
    UnmatchedBlockCommentStart {
        /// Returns the location of the block terminator found
        location: Location,
    },
}

impl fmt::Display for CommentError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CommentError::UnmatchedBlockCommentStart { location } => {
                write!(
                    f,
                    "unmatched block comment start at line {}, column {}",
                    location.line(),
                    location.column()
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
    /// - the span of the comment as a [`CommentSpan`]
    #[must_use]
    pub const fn new(comment: Comment, span: Span) -> Self {
        Self { comment, span }
    }

    /// Getter method for retrieving the comment content
    #[must_use]
    pub const fn comment(&self) -> &Comment {
        &self.comment
    }

    /// Getter method for retrieving the [`CommentSpan`] of the comment
    #[must_use]
    pub const fn span(&self) -> &Span {
        &self.span
    }
}

/// Structure for holding all comments found in the document
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
    #[must_use]
    pub fn parse_all_comments_from_file(file: &ParsedSqlFile) -> CommentResult<Self> {
        let src = file.content();
        let mut comments = Vec::new();
        let mut current_location = Location::default();

        Ok(Self { comments })
    }

    /// Scans the raw file and collects all comments
    ///
    /// # Parameters
    /// - `src` which is the `SQL` file content as a [`str`]
    fn scan_comments(src: &str) -> Self {
        let mut comments = Vec::new();

        Self { comments }
    }

    /// Parse single line comments

    /// Parse multi line comments

    /// Getter method for retrieving the Vec of [`Comment`]
    #[must_use]
    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }
}
