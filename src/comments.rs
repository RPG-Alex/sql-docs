//! Module for crawling the SQL documents based on the parser and
//! parsing/extracting the leading comments.
//!
//! *leading comment* a comment that
//! precedes an SQL Statement.
use std::{fmt, path::Path};

use crate::{ast::ParsedSqlFile, files::{SqlFile, SqlFilesList}};

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
        Self::new(0, 0)
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
    /// Found a multiline comment terminator `*/` without a matching opener `/*`
    UnmatchedMultilineCommentStart {
        /// Returns the location of the terminator found
        location: Location,
    },
    /// Found a multiline comment that is not properly terminated before EOF
    UnterminatedMultiLineComment {
        /// Returns the location of where the multiline comment started
        start: Location
    }
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
            },
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
    ///
    /// # Parameters
    /// - `file`: the [`ParsedSqlFile`] that needs to be parsed for comments
    ///
    /// # Errors
    /// - Will return [`CommentError::UnmatchedBlockCommentStart`] if a comment
    ///   does not have an opening `/*`
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
    /// - `UnmatchedMultilineCommentStart` : will return error if unable to find a starting `/*` for a multiline comment
    /// - `UnterminatedMultiLineComment` : will return error if there is an unterminated multiline comment, found at EOF
    pub fn scan_comments(src: &str) -> CommentResult<Self> {
        let mut comments = Vec::new();

        let mut start_line = 0u64;
        let mut start_col = 0u64;

        let mut line = 0u64;
        let mut col = 0u64;

        let mut in_single = false;
        let mut in_block = false;


        let mut buf = String::new();

        let mut chars = src.chars().peekable();


        while let Some(c) = chars.next() {
            match (in_single, in_block, c) {
                (false, false, '-') => {
                    if chars.peek().copied() == Some('-') {
                        chars.next(); 
                        in_single = true;
                        start_line = line;
                        start_col = col; 
                        buf.clear();
                        buf.push('-');
                        buf.push('-');
                        col += 1;
                    }
                },
                (false, false, '/') => {
                    if chars.peek().copied() == Some('*') {
                        chars.next();
                        in_block = true;
                        start_line = line;
                        start_col = col; 
                        buf.clear();
                        col += 1; 
                    }
                },
                (false, false, '*') => {
                    if chars.peek().copied() == Some('*') {
                        let loc = Location::new(line, col);
                        return Err(CommentError::UnmatchedMultilineCommentStart { location: loc });
                    }
                },
                (true, false, '\n') => {
                    let end_loc = Location::new(line, col);
                    comments.push(Comment::new(
                        CommentKind::SingleLine(buf.clone()),
                        Span::new(Location { line: start_line, column: start_col}, end_loc),
                    ));
                    in_single = false;
                    buf.clear();
                },
                (false, true, '*') => {
                    if chars.peek().copied() == Some('/'){
                        chars.next();
                        buf.push('*'); 
                        buf.push('/'); 
                        let end_loc = Location::new(line, col + 1);
                        comments.push(Comment::new(
                            CommentKind::MultiLine(buf.trim().to_string()),
                            Span::new(Location { line: start_line, column: start_col}, end_loc),
                        ));
                        in_block = false;
                        buf.clear();
                        col += 1;
                    } else {
                        buf.push('*');
                    }    
                },
                (false, true, '\n') => {
                    buf.push('\n');
                },
                (false, true, ch) | (true, false, ch)=> {
                    buf.push(ch);
                },
                (_, _, _) => unreachable!("cannot be in single-line and block comment at once"),
            }
            if c == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }

        // EOF: close any open comments
        if in_block {
            return Err(CommentError::UnterminatedMultiLineComment { start: Location { line: start_line, column: start_col} });
        }

        if in_single && !buf.is_empty() {
            let end_loc = Location::new(line, col);
            comments.push(Comment::new(
                CommentKind::SingleLine(buf.trim_end().to_string()),
                Span::new(Location { line: start_line, column: start_col}, end_loc),
            ));
        }

        Ok(Self { comments })
    }

    /// Getter method for retrieving the Vec of [`Comment`]
    #[must_use]
    pub fn comments(&self) -> &[Comment] {
        &self.comments
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
    assert_eq!(location2, Location { line: 0, column: 0 });
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
        Span::new(Location { line: 0, column: 0 }, Location { line: 0, column: len - 1 });

    assert_eq!(comment.span, expected_span);
}

#[test]
fn multiline_comment_span() {
    let kind = CommentKind::MultiLine("/* hello\nworld */".to_string());
    let span = Span::new(Location { line: 1, column: 0 }, Location { line: 2, column: 9 });

    let comment = Comment::new(kind.clone(), span);

    assert_eq!(comment.kind, kind);
    assert_eq!(comment.span.start.line, 1);
    assert_eq!(comment.span.end.line, 2);
}

#[test]
fn parse_single_line_comments() {
    use std::fs;
    use crate::files::SqlFileSet;
    use crate::ast::ParsedSqlFileSet;
    let path = Path::new("sql_files");
    let set = SqlFileSet::new(path, None).unwrap();
    let parsed_set = ParsedSqlFileSet::parse_all(set).unwrap();
    for file in parsed_set.files().iter() {
        let parsed_comments = Comments::parse_all_comments_from_file(file).unwrap(); 
        match file.file().path().to_str().unwrap() {
            "sql_files/with_comments.sql" => {
                println!("succeeded!");
            },
            "sql_files/without_comments.sql" => {

            },
            _ => unreachable!("This shouldn't be accessible if directory parsed correctly")
        }
    }


}


#[test]
fn parse_multi_line_comments() {
    
}