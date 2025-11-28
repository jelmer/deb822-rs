//! Parser for deb822 style files.
//!
//! This parser can be used to parse files in the deb822 format, while preserving
//! all whitespace and comments. It is based on the [rowan] library, which is a
//! lossless parser library for Rust.
//!
//! Once parsed, the file can be traversed or modified, and then written back to
//! a file.
//!
//! # Example
//!
//! ```rust
//! use deb822_lossless::Deb822;
//! use std::str::FromStr;
//!
//! let input = r###"Package: deb822-lossless
//! ## Comments are preserved
//! Maintainer: Jelmer Vernooĳ <jelmer@debian.org>
//! Homepage: https://github.com/jelmer/deb822-lossless
//! Section: rust
//!
//! Package: deb822-lossless
//! Architecture: any
//! Description: Lossless parser for deb822 style files.
//!   This parser can be used to parse files in the deb822 format, while preserving
//!   all whitespace and comments. It is based on the [rowan] library, which is a
//!   lossless parser library for Rust.
//! "###;
//!
//! let deb822 = Deb822::from_str(input).unwrap();
//! assert_eq!(deb822.paragraphs().count(), 2);
//! let homepage = deb822.paragraphs().nth(0).unwrap().get("Homepage");
//! assert_eq!(homepage.as_deref(), Some("https://github.com/jelmer/deb822-lossless"));
//! ```

use crate::{
    lex::lex,
    lex::SyntaxKind::{self, *},
    Indentation,
};
use rowan::ast::AstNode;
use std::path::Path;
use std::str::FromStr;

/// A positioned parse error containing location information.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PositionedParseError {
    /// The error message
    pub message: String,
    /// The text range where the error occurred
    pub range: rowan::TextRange,
    /// Optional error code for categorization
    pub code: Option<String>,
}

impl std::fmt::Display for PositionedParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for PositionedParseError {}

/// List of encountered syntax errors.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseError(pub Vec<String>);

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for err in &self.0 {
            writeln!(f, "{}", err)?;
        }
        Ok(())
    }
}

impl std::error::Error for ParseError {}

/// Error parsing deb822 control files
#[derive(Debug)]
pub enum Error {
    /// A syntax error was encountered while parsing the file.
    ParseError(ParseError),

    /// An I/O error was encountered while reading the file.
    IoError(std::io::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self {
            Error::ParseError(err) => write!(f, "{}", err),
            Error::IoError(err) => write!(f, "{}", err),
        }
    }
}

impl From<ParseError> for Error {
    fn from(err: ParseError) -> Self {
        Self::ParseError(err)
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Self::IoError(err)
    }
}

impl std::error::Error for Error {}

/// Second, implementing the `Language` trait teaches rowan to convert between
/// these two SyntaxKind types, allowing for a nicer SyntaxNode API where
/// "kinds" are values from our `enum SyntaxKind`, instead of plain u16 values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}
impl rowan::Language for Lang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

/// GreenNode is an immutable tree, which is cheap to change,
/// but doesn't contain offsets and parent pointers.
use rowan::GreenNode;

/// You can construct GreenNodes by hand, but a builder
/// is helpful for top-down parsers: it maintains a stack
/// of currently in-progress nodes
use rowan::GreenNodeBuilder;

/// The parse results are stored as a "green tree".
/// We'll discuss working with the results later
pub(crate) struct Parse {
    pub(crate) green_node: GreenNode,
    #[allow(unused)]
    pub(crate) errors: Vec<String>,
    pub(crate) positioned_errors: Vec<PositionedParseError>,
}

pub(crate) fn parse(text: &str) -> Parse {
    struct Parser<'a> {
        /// input tokens, including whitespace,
        /// in *reverse* order.
        tokens: Vec<(SyntaxKind, &'a str)>,
        /// the in-progress tree.
        builder: GreenNodeBuilder<'static>,
        /// the list of syntax errors we've accumulated
        /// so far.
        errors: Vec<String>,
        /// positioned errors with location information
        positioned_errors: Vec<PositionedParseError>,
        /// All tokens with their positions in forward order for position tracking
        token_positions: Vec<(SyntaxKind, rowan::TextSize, rowan::TextSize)>,
        /// current token index (counting from the end since tokens are in reverse)
        current_token_index: usize,
    }

    impl<'a> Parser<'a> {
        /// Skip to next paragraph boundary for error recovery
        fn skip_to_paragraph_boundary(&mut self) {
            while self.current().is_some() {
                match self.current() {
                    Some(NEWLINE) => {
                        self.bump();
                        // Check if next line starts a new paragraph (key at start of line)
                        if self.at_paragraph_start() {
                            break;
                        }
                    }
                    _ => {
                        self.bump();
                    }
                }
            }
        }

        /// Check if we're at the start of a new paragraph
        fn at_paragraph_start(&self) -> bool {
            match self.current() {
                Some(KEY) => true,
                Some(COMMENT) => true,
                None => true, // EOF is a valid paragraph boundary
                _ => false,
            }
        }

        /// Attempt to recover from entry parsing errors
        fn recover_entry(&mut self) {
            // Skip to end of current line
            while self.current().is_some() && self.current() != Some(NEWLINE) {
                self.bump();
            }
            // Consume the newline if present
            if self.current() == Some(NEWLINE) {
                self.bump();
            }
        }
        fn parse_entry(&mut self) {
            // Handle leading comments
            while self.current() == Some(COMMENT) {
                self.bump();

                match self.current() {
                    Some(NEWLINE) => {
                        self.bump();
                    }
                    None => {
                        return;
                    }
                    Some(g) => {
                        self.builder.start_node(ERROR.into());
                        self.add_positioned_error(
                            format!("expected newline after comment, got {g:?}"),
                            Some("unexpected_token_after_comment".to_string()),
                        );
                        self.bump();
                        self.builder.finish_node();
                        self.recover_entry();
                        return;
                    }
                }
            }

            self.builder.start_node(ENTRY.into());
            let mut entry_has_errors = false;

            // Parse the key
            if self.current() == Some(KEY) {
                self.bump();
                self.skip_ws();
            } else {
                entry_has_errors = true;
                self.builder.start_node(ERROR.into());

                // Enhanced error recovery for malformed keys
                match self.current() {
                    Some(VALUE) | Some(WHITESPACE) => {
                        self.add_positioned_error(
                            "field name cannot start with whitespace or special characters"
                                .to_string(),
                            Some("invalid_field_name".to_string()),
                        );
                        // Try to consume what might be an intended key
                        while self.current() == Some(VALUE) || self.current() == Some(WHITESPACE) {
                            self.bump();
                        }
                    }
                    Some(COLON) => {
                        self.add_positioned_error(
                            "field name missing before colon".to_string(),
                            Some("missing_field_name".to_string()),
                        );
                    }
                    Some(NEWLINE) => {
                        self.add_positioned_error(
                            "empty line where field expected".to_string(),
                            Some("empty_field_line".to_string()),
                        );
                        self.builder.finish_node();
                        self.builder.finish_node();
                        return;
                    }
                    _ => {
                        self.add_positioned_error(
                            format!("expected field name, got {:?}", self.current()),
                            Some("missing_key".to_string()),
                        );
                        if self.current().is_some() {
                            self.bump();
                        }
                    }
                }
                self.builder.finish_node();
            }

            // Parse the colon
            if self.current() == Some(COLON) {
                self.bump();
                self.skip_ws();
            } else {
                entry_has_errors = true;
                self.builder.start_node(ERROR.into());

                // Enhanced error recovery for missing colon
                match self.current() {
                    Some(VALUE) => {
                        self.add_positioned_error(
                            "missing colon ':' after field name".to_string(),
                            Some("missing_colon".to_string()),
                        );
                        // Don't consume the value, let it be parsed as the field value
                    }
                    Some(NEWLINE) => {
                        self.add_positioned_error(
                            "field name without value (missing colon and value)".to_string(),
                            Some("incomplete_field".to_string()),
                        );
                        self.builder.finish_node();
                        self.builder.finish_node();
                        return;
                    }
                    Some(KEY) => {
                        self.add_positioned_error(
                            "field name followed by another field name (missing colon and value)"
                                .to_string(),
                            Some("consecutive_field_names".to_string()),
                        );
                        // Don't consume the next key, let it be parsed as a new entry
                        self.builder.finish_node();
                        self.builder.finish_node();
                        return;
                    }
                    _ => {
                        self.add_positioned_error(
                            format!("expected colon ':', got {:?}", self.current()),
                            Some("missing_colon".to_string()),
                        );
                        if self.current().is_some() {
                            self.bump();
                        }
                    }
                }
                self.builder.finish_node();
            }

            // Parse the value (potentially multi-line)
            loop {
                while self.current() == Some(WHITESPACE) || self.current() == Some(VALUE) {
                    self.bump();
                }

                match self.current() {
                    None => {
                        break;
                    }
                    Some(NEWLINE) => {
                        self.bump();
                    }
                    Some(KEY) => {
                        // We've hit another field, this entry is complete
                        break;
                    }
                    Some(g) => {
                        self.builder.start_node(ERROR.into());
                        self.add_positioned_error(
                            format!("unexpected token in field value: {g:?}"),
                            Some("unexpected_value_token".to_string()),
                        );
                        self.bump();
                        self.builder.finish_node();
                    }
                }

                // Check for continuation lines
                if self.current() == Some(INDENT) {
                    self.bump();
                    self.skip_ws();
                } else {
                    break;
                }
            }

            self.builder.finish_node();

            // If the entry had errors, we might want to recover
            if entry_has_errors && !self.at_paragraph_start() && self.current().is_some() {
                self.recover_entry();
            }
        }

        fn parse_paragraph(&mut self) {
            self.builder.start_node(PARAGRAPH.into());

            let mut consecutive_errors = 0;
            const MAX_CONSECUTIVE_ERRORS: usize = 5;

            while self.current() != Some(NEWLINE) && self.current().is_some() {
                let error_count_before = self.positioned_errors.len();

                // Check if we're at a valid entry start
                if self.current() == Some(KEY) || self.current() == Some(COMMENT) {
                    self.parse_entry();

                    // Reset consecutive error count if we successfully parsed something
                    if self.positioned_errors.len() == error_count_before {
                        consecutive_errors = 0;
                    } else {
                        consecutive_errors += 1;
                    }
                } else {
                    // We're not at a valid entry start, this is an error
                    consecutive_errors += 1;

                    self.builder.start_node(ERROR.into());
                    match self.current() {
                        Some(VALUE) => {
                            self.add_positioned_error(
                                "orphaned text without field name".to_string(),
                                Some("orphaned_text".to_string()),
                            );
                            // Consume the orphaned text
                            while self.current() == Some(VALUE)
                                || self.current() == Some(WHITESPACE)
                            {
                                self.bump();
                            }
                        }
                        Some(COLON) => {
                            self.add_positioned_error(
                                "orphaned colon without field name".to_string(),
                                Some("orphaned_colon".to_string()),
                            );
                            self.bump();
                        }
                        Some(INDENT) => {
                            self.add_positioned_error(
                                "unexpected indentation without field".to_string(),
                                Some("unexpected_indent".to_string()),
                            );
                            self.bump();
                        }
                        _ => {
                            self.add_positioned_error(
                                format!(
                                    "unexpected token at paragraph level: {:?}",
                                    self.current()
                                ),
                                Some("unexpected_paragraph_token".to_string()),
                            );
                            self.bump();
                        }
                    }
                    self.builder.finish_node();
                }

                // If we have too many consecutive errors, skip to paragraph boundary
                if consecutive_errors >= MAX_CONSECUTIVE_ERRORS {
                    self.add_positioned_error(
                        "too many consecutive parse errors, skipping to next paragraph".to_string(),
                        Some("parse_recovery".to_string()),
                    );
                    self.skip_to_paragraph_boundary();
                    break;
                }
            }

            self.builder.finish_node();
        }

        fn parse(mut self) -> Parse {
            // Make sure that the root node covers all source
            self.builder.start_node(ROOT.into());
            while self.current().is_some() {
                self.skip_ws_and_newlines();
                if self.current().is_some() {
                    self.parse_paragraph();
                }
            }
            // Don't forget to eat *trailing* whitespace
            self.skip_ws_and_newlines();
            // Close the root node.
            self.builder.finish_node();

            // Turn the builder into a GreenNode
            Parse {
                green_node: self.builder.finish(),
                errors: self.errors,
                positioned_errors: self.positioned_errors,
            }
        }
        /// Advance one token, adding it to the current branch of the tree builder.
        fn bump(&mut self) {
            let (kind, text) = self.tokens.pop().unwrap();
            self.builder.token(kind.into(), text);
            if self.current_token_index > 0 {
                self.current_token_index -= 1;
            }
        }
        /// Peek at the first unprocessed token
        fn current(&self) -> Option<SyntaxKind> {
            self.tokens.last().map(|(kind, _)| *kind)
        }

        /// Add a positioned error at the current position
        fn add_positioned_error(&mut self, message: String, code: Option<String>) {
            let range = if self.current_token_index < self.token_positions.len() {
                let (_, start, end) = self.token_positions[self.current_token_index];
                rowan::TextRange::new(start, end)
            } else {
                // Default to end of text if no current token
                let end = self
                    .token_positions
                    .last()
                    .map(|(_, _, end)| *end)
                    .unwrap_or_else(|| rowan::TextSize::from(0));
                rowan::TextRange::new(end, end)
            };

            self.positioned_errors.push(PositionedParseError {
                message: message.clone(),
                range,
                code,
            });
            self.errors.push(message);
        }
        fn skip_ws(&mut self) {
            while self.current() == Some(WHITESPACE) || self.current() == Some(COMMENT) {
                self.bump()
            }
        }
        fn skip_ws_and_newlines(&mut self) {
            while self.current() == Some(WHITESPACE)
                || self.current() == Some(COMMENT)
                || self.current() == Some(NEWLINE)
            {
                self.builder.start_node(EMPTY_LINE.into());
                while self.current() != Some(NEWLINE) && self.current().is_some() {
                    self.bump();
                }
                if self.current() == Some(NEWLINE) {
                    self.bump();
                }
                self.builder.finish_node();
            }
        }
    }

    let mut tokens = lex(text).collect::<Vec<_>>();

    // Build token positions in forward order
    let mut token_positions = Vec::new();
    let mut position = rowan::TextSize::from(0);
    for (kind, text) in &tokens {
        let start = position;
        let end = start + rowan::TextSize::of(*text);
        token_positions.push((*kind, start, end));
        position = end;
    }

    // Reverse tokens for parsing (but keep positions in forward order)
    tokens.reverse();
    let current_token_index = tokens.len().saturating_sub(1);

    Parser {
        tokens,
        builder: GreenNodeBuilder::new(),
        errors: Vec::new(),
        positioned_errors: Vec::new(),
        token_positions,
        current_token_index,
    }
    .parse()
}

/// To work with the parse results we need a view into the
/// green tree - the Syntax tree.
/// It is also immutable, like a GreenNode,
/// but it contains parent pointers, offsets, and
/// has identity semantics.

type SyntaxNode = rowan::SyntaxNode<Lang>;
#[allow(unused)]
type SyntaxToken = rowan::SyntaxToken<Lang>;
#[allow(unused)]
type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

impl Parse {
    #[cfg(test)]
    fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    fn root_mut(&self) -> Deb822 {
        Deb822::cast(SyntaxNode::new_root_mut(self.green_node.clone())).unwrap()
    }
}

macro_rules! ast_node {
    ($ast:ident, $kind:ident) => {
        #[doc = "An AST node representing a `"]
        #[doc = stringify!($ast)]
        #[doc = "`."]
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        #[repr(transparent)]
        pub struct $ast(SyntaxNode);
        impl $ast {
            #[allow(unused)]
            fn cast(node: SyntaxNode) -> Option<Self> {
                if node.kind() == $kind {
                    Some(Self(node))
                } else {
                    None
                }
            }
        }

        impl AstNode for $ast {
            type Language = Lang;

            fn can_cast(kind: SyntaxKind) -> bool {
                kind == $kind
            }

            fn cast(syntax: SyntaxNode) -> Option<Self> {
                Self::cast(syntax)
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.0
            }
        }

        impl std::fmt::Display for $ast {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "{}", self.0.text())
            }
        }
    };
}

ast_node!(Deb822, ROOT);
ast_node!(Paragraph, PARAGRAPH);
ast_node!(Entry, ENTRY);

impl Default for Deb822 {
    fn default() -> Self {
        Self::new()
    }
}

impl Deb822 {
    /// Create a new empty deb822 file.
    pub fn new() -> Deb822 {
        let mut builder = GreenNodeBuilder::new();

        builder.start_node(ROOT.into());
        builder.finish_node();
        Deb822(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Parse deb822 text, returning a Parse result
    pub fn parse(text: &str) -> crate::Parse<Deb822> {
        crate::Parse::parse_deb822(text)
    }

    /// Provide a formatter that can handle indentation and trailing separators
    ///
    /// # Arguments
    /// * `control` - The control file to format
    /// * `indentation` - The indentation to use
    /// * `immediate_empty_line` - Whether the value should always start with an empty line. If true,
    ///                  then the result becomes something like "Field:\n value". This parameter
    ///                  only applies to the values that will be formatted over more than one line.
    /// * `max_line_length_one_liner` - If set, then this is the max length of the value
    ///                        if it is crammed into a "one-liner" value. If the value(s) fit into
    ///                        one line, this parameter will overrule immediate_empty_line.
    /// * `sort_paragraphs` - If set, then this function will sort the paragraphs according to the
    ///                given function.
    /// * `sort_entries` - If set, then this function will sort the entries according to the
    ///               given function.
    #[must_use]
    pub fn wrap_and_sort(
        &self,
        sort_paragraphs: Option<&dyn Fn(&Paragraph, &Paragraph) -> std::cmp::Ordering>,
        wrap_and_sort_paragraph: Option<&dyn Fn(&Paragraph) -> Paragraph>,
    ) -> Deb822 {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(ROOT.into());
        let mut current = vec![];
        let mut paragraphs = vec![];
        for c in self.0.children_with_tokens() {
            match c.kind() {
                PARAGRAPH => {
                    paragraphs.push((
                        current,
                        Paragraph::cast(c.as_node().unwrap().clone()).unwrap(),
                    ));
                    current = vec![];
                }
                COMMENT | ERROR => {
                    current.push(c);
                }
                EMPTY_LINE => {
                    current.extend(
                        c.as_node()
                            .unwrap()
                            .children_with_tokens()
                            .skip_while(|c| matches!(c.kind(), EMPTY_LINE | NEWLINE | WHITESPACE)),
                    );
                }
                _ => {}
            }
        }
        if let Some(sort_paragraph) = sort_paragraphs {
            paragraphs.sort_by(|a, b| {
                let a_key = &a.1;
                let b_key = &b.1;
                sort_paragraph(a_key, b_key)
            });
        }

        for (i, paragraph) in paragraphs.into_iter().enumerate() {
            if i > 0 {
                builder.start_node(EMPTY_LINE.into());
                builder.token(NEWLINE.into(), "\n");
                builder.finish_node();
            }
            for c in paragraph.0.into_iter() {
                builder.token(c.kind().into(), c.as_token().unwrap().text());
            }
            let new_paragraph = if let Some(ref ws) = wrap_and_sort_paragraph {
                ws(&paragraph.1)
            } else {
                paragraph.1
            };
            inject(&mut builder, new_paragraph.0);
        }

        for c in current {
            builder.token(c.kind().into(), c.as_token().unwrap().text());
        }

        builder.finish_node();
        Self(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Normalize the spacing around field separators (colons) for all entries in all paragraphs in place.
    ///
    /// This ensures that there is exactly one space after the colon and before the value
    /// for each field in every paragraph. This is a lossless operation that preserves the
    /// field names, values, and comments, but normalizes the whitespace formatting.
    ///
    /// # Examples
    ///
    /// ```
    /// use deb822_lossless::Deb822;
    /// use std::str::FromStr;
    ///
    /// let input = "Field1:    value1\nField2:value2\n\nField3:  value3\n";
    /// let mut deb822 = Deb822::from_str(input).unwrap();
    ///
    /// deb822.normalize_field_spacing();
    /// assert_eq!(deb822.to_string(), "Field1: value1\nField2: value2\n\nField3: value3\n");
    /// ```
    pub fn normalize_field_spacing(&mut self) {
        // Collect paragraph indices and iterate through them
        let para_indices: Vec<_> = self
            .0
            .children()
            .filter_map(|c| {
                if c.kind() == PARAGRAPH {
                    Some(c.index())
                } else {
                    None
                }
            })
            .collect();

        for para_index in para_indices {
            if let Some(para_node) = self.0.children().nth(para_index) {
                let mut para = Paragraph::cast(para_node).unwrap();
                para.normalize_field_spacing();
            }
        }
    }

    /// Returns an iterator over all paragraphs in the file.
    pub fn paragraphs(&self) -> impl Iterator<Item = Paragraph> {
        self.0.children().filter_map(Paragraph::cast)
    }

    /// Converts the perceptual paragraph index to the node index.
    fn convert_index(&self, index: usize) -> Option<usize> {
        let mut current_pos = 0usize;
        if index == 0 {
            return Some(0);
        }
        for (i, node) in self.0.children_with_tokens().enumerate() {
            if node.kind() == PARAGRAPH {
                if current_pos == index {
                    return Some(i);
                }
                current_pos += 1;
            }
        }

        None
    }

    /// Delete trailing empty lines after specified node and before any non-empty line nodes.
    fn delete_trailing_space(&self, start: usize) {
        for (i, node) in self.0.children_with_tokens().enumerate() {
            if i < start {
                continue;
            }
            if node.kind() != EMPTY_LINE {
                return;
            }
            // this is not a typo, the index will shift by one after deleting the node
            // so instead of deleting using `i`, we use `start` as the start index
            self.0.splice_children(start..start + 1, []);
        }
    }

    /// Shared internal function to insert a new paragraph into the file.
    fn insert_empty_paragraph(&mut self, index: Option<usize>) -> Paragraph {
        let paragraph = Paragraph::new();
        let mut to_insert = vec![];
        if self.0.children().count() > 0 {
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(EMPTY_LINE.into());
            builder.token(NEWLINE.into(), "\n");
            builder.finish_node();
            to_insert.push(SyntaxNode::new_root_mut(builder.finish()).into());
        }
        to_insert.push(paragraph.0.clone().into());
        let insertion_point = match index {
            Some(i) => {
                if to_insert.len() > 1 {
                    to_insert.swap(0, 1);
                }
                i
            }
            None => self.0.children().count(),
        };
        self.0
            .splice_children(insertion_point..insertion_point, to_insert);
        paragraph
    }

    /// Insert a new empty paragraph into the file after specified index.
    ///
    /// # Examples
    ///
    /// ```
    /// use deb822_lossless::{Deb822, Paragraph};
    /// let mut d: Deb822 = vec![
    ///     vec![("Foo", "Bar"), ("Baz", "Qux")].into_iter().collect(),
    ///     vec![("A", "B"), ("C", "D")].into_iter().collect(),
    /// ]
    /// .into_iter()
    /// .collect();
    /// let mut p = d.insert_paragraph(0);
    /// p.set("Foo", "Baz");
    /// assert_eq!(d.to_string(), "Foo: Baz\n\nFoo: Bar\nBaz: Qux\n\nA: B\nC: D\n");
    /// let mut another = d.insert_paragraph(1);
    /// another.set("Y", "Z");
    /// assert_eq!(d.to_string(), "Foo: Baz\n\nY: Z\n\nFoo: Bar\nBaz: Qux\n\nA: B\nC: D\n");
    /// ```
    pub fn insert_paragraph(&mut self, index: usize) -> Paragraph {
        self.insert_empty_paragraph(self.convert_index(index))
    }

    /// Remove the paragraph at the specified index from the file.
    ///
    /// # Examples
    ///
    /// ```
    /// use deb822_lossless::Deb822;
    /// let mut d: Deb822 = vec![
    ///     vec![("Foo", "Bar"), ("Baz", "Qux")].into_iter().collect(),
    ///     vec![("A", "B"), ("C", "D")].into_iter().collect(),
    /// ]
    /// .into_iter()
    /// .collect();
    /// d.remove_paragraph(0);
    /// assert_eq!(d.to_string(), "A: B\nC: D\n");
    /// d.remove_paragraph(0);
    /// assert_eq!(d.to_string(), "");
    /// ```
    pub fn remove_paragraph(&mut self, index: usize) {
        if let Some(index) = self.convert_index(index) {
            self.0.splice_children(index..index + 1, []);
            self.delete_trailing_space(index);
        }
    }

    /// Move a paragraph from one index to another.
    ///
    /// This moves the paragraph at `from_index` to `to_index`, shifting other paragraphs as needed.
    /// If `from_index` equals `to_index`, no operation is performed.
    ///
    /// # Examples
    ///
    /// ```
    /// use deb822_lossless::Deb822;
    /// let mut d: Deb822 = vec![
    ///     vec![("Foo", "Bar"), ("Baz", "Qux")].into_iter().collect(),
    ///     vec![("A", "B"), ("C", "D")].into_iter().collect(),
    ///     vec![("X", "Y"), ("Z", "W")].into_iter().collect(),
    /// ]
    /// .into_iter()
    /// .collect();
    /// d.move_paragraph(0, 2);
    /// assert_eq!(d.to_string(), "A: B\nC: D\n\nX: Y\nZ: W\n\nFoo: Bar\nBaz: Qux\n");
    /// ```
    pub fn move_paragraph(&mut self, from_index: usize, to_index: usize) {
        if from_index == to_index {
            return;
        }

        // Get the paragraph count to validate indices
        let paragraph_count = self.paragraphs().count();
        if from_index >= paragraph_count || to_index >= paragraph_count {
            return;
        }

        // Clone the paragraph node we want to move
        let paragraph_to_move = self.paragraphs().nth(from_index).unwrap().0.clone();

        // Remove the paragraph from its original position
        let from_physical = self.convert_index(from_index).unwrap();

        // Determine the range to remove (paragraph and possibly preceding EMPTY_LINE)
        let mut start_idx = from_physical;
        if from_physical > 0 {
            if let Some(prev_node) = self.0.children_with_tokens().nth(from_physical - 1) {
                if prev_node.kind() == EMPTY_LINE {
                    start_idx = from_physical - 1;
                }
            }
        }

        // Remove the paragraph and any preceding EMPTY_LINE
        self.0.splice_children(start_idx..from_physical + 1, []);
        self.delete_trailing_space(start_idx);

        // Calculate the physical insertion point
        // After removal, we need to determine where to insert
        // The semantics are: the moved paragraph ends up at logical index to_index in the final result
        let insert_at = if to_index > from_index {
            // Moving forward: after removal, to_index-1 paragraphs should be before the moved one
            // So we insert after paragraph at index (to_index - 1)
            let target_idx = to_index - 1;
            if let Some(target_physical) = self.convert_index(target_idx) {
                target_physical + 1
            } else {
                // If convert_index returns None, insert at the end
                self.0.children().count()
            }
        } else {
            // Moving backward: after removal, to_index paragraphs should be before the moved one
            // So we insert at paragraph index to_index
            if let Some(target_physical) = self.convert_index(to_index) {
                target_physical
            } else {
                self.0.children().count()
            }
        };

        // Build the nodes to insert
        let mut to_insert = vec![];

        // Determine if we need to add an EMPTY_LINE before the paragraph
        let needs_empty_line_before = if insert_at == 0 {
            // At the beginning - no empty line before
            false
        } else if insert_at > 0 {
            // Check if there's already an EMPTY_LINE at the insertion point
            if let Some(node_at_insert) = self.0.children_with_tokens().nth(insert_at - 1) {
                node_at_insert.kind() != EMPTY_LINE
            } else {
                false
            }
        } else {
            false
        };

        if needs_empty_line_before {
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(EMPTY_LINE.into());
            builder.token(NEWLINE.into(), "\n");
            builder.finish_node();
            to_insert.push(SyntaxNode::new_root_mut(builder.finish()).into());
        }

        to_insert.push(paragraph_to_move.into());

        // Determine if we need to add an EMPTY_LINE after the paragraph
        let needs_empty_line_after = if insert_at < self.0.children().count() {
            // There are nodes after - check if next node is EMPTY_LINE
            if let Some(node_after) = self.0.children_with_tokens().nth(insert_at) {
                node_after.kind() != EMPTY_LINE
            } else {
                false
            }
        } else {
            false
        };

        if needs_empty_line_after {
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(EMPTY_LINE.into());
            builder.token(NEWLINE.into(), "\n");
            builder.finish_node();
            to_insert.push(SyntaxNode::new_root_mut(builder.finish()).into());
        }

        // Insert at the new position
        self.0.splice_children(insert_at..insert_at, to_insert);
    }

    /// Add a new empty paragraph to the end of the file.
    pub fn add_paragraph(&mut self) -> Paragraph {
        self.insert_empty_paragraph(None)
    }

    /// Swap two paragraphs by their indices.
    ///
    /// This method swaps the positions of two paragraphs while preserving their
    /// content, formatting, whitespace, and comments. The paragraphs at positions
    /// `index1` and `index2` will exchange places.
    ///
    /// # Arguments
    ///
    /// * `index1` - The index of the first paragraph to swap
    /// * `index2` - The index of the second paragraph to swap
    ///
    /// # Panics
    ///
    /// Panics if either `index1` or `index2` is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use deb822_lossless::Deb822;
    /// let mut d: Deb822 = vec![
    ///     vec![("Foo", "Bar")].into_iter().collect(),
    ///     vec![("A", "B")].into_iter().collect(),
    ///     vec![("X", "Y")].into_iter().collect(),
    /// ]
    /// .into_iter()
    /// .collect();
    /// d.swap_paragraphs(0, 2);
    /// assert_eq!(d.to_string(), "X: Y\n\nA: B\n\nFoo: Bar\n");
    /// ```
    pub fn swap_paragraphs(&mut self, index1: usize, index2: usize) {
        if index1 == index2 {
            return;
        }

        // Collect all children
        let mut children: Vec<_> = self.0.children().map(|n| n.clone().into()).collect();

        // Find the child indices for paragraphs
        let mut para_child_indices = vec![];
        for (child_idx, child) in self.0.children().enumerate() {
            if child.kind() == PARAGRAPH {
                para_child_indices.push(child_idx);
            }
        }

        // Validate paragraph indices
        if index1 >= para_child_indices.len() {
            panic!("index1 {} out of bounds", index1);
        }
        if index2 >= para_child_indices.len() {
            panic!("index2 {} out of bounds", index2);
        }

        let child_idx1 = para_child_indices[index1];
        let child_idx2 = para_child_indices[index2];

        // Swap the children in the vector
        children.swap(child_idx1, child_idx2);

        // Replace all children
        let num_children = children.len();
        self.0.splice_children(0..num_children, children);
    }

    /// Read a deb822 file from the given path.
    pub fn from_file(path: impl AsRef<Path>) -> Result<Self, Error> {
        let text = std::fs::read_to_string(path)?;
        Ok(Self::from_str(&text)?)
    }

    /// Read a deb822 file from the given path, ignoring any syntax errors.
    pub fn from_file_relaxed(
        path: impl AsRef<Path>,
    ) -> Result<(Self, Vec<String>), std::io::Error> {
        let text = std::fs::read_to_string(path)?;
        Ok(Self::from_str_relaxed(&text))
    }

    /// Parse a deb822 file from a string, allowing syntax errors.
    pub fn from_str_relaxed(s: &str) -> (Self, Vec<String>) {
        let parsed = parse(s);
        (parsed.root_mut(), parsed.errors)
    }

    /// Read a deb822 file from a Read object.
    pub fn read<R: std::io::Read>(mut r: R) -> Result<Self, Error> {
        let mut buf = String::new();
        r.read_to_string(&mut buf)?;
        Ok(Self::from_str(&buf)?)
    }

    /// Read a deb822 file from a Read object, allowing syntax errors.
    pub fn read_relaxed<R: std::io::Read>(mut r: R) -> Result<(Self, Vec<String>), std::io::Error> {
        let mut buf = String::new();
        r.read_to_string(&mut buf)?;
        Ok(Self::from_str_relaxed(&buf))
    }
}

fn inject(builder: &mut GreenNodeBuilder, node: SyntaxNode) {
    builder.start_node(node.kind().into());
    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Node(child) => {
                inject(builder, child);
            }
            rowan::NodeOrToken::Token(token) => {
                builder.token(token.kind().into(), token.text());
            }
        }
    }
    builder.finish_node();
}

impl FromIterator<Paragraph> for Deb822 {
    fn from_iter<T: IntoIterator<Item = Paragraph>>(iter: T) -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(ROOT.into());
        for (i, paragraph) in iter.into_iter().enumerate() {
            if i > 0 {
                builder.start_node(EMPTY_LINE.into());
                builder.token(NEWLINE.into(), "\n");
                builder.finish_node();
            }
            inject(&mut builder, paragraph.0);
        }
        builder.finish_node();
        Self(SyntaxNode::new_root_mut(builder.finish()))
    }
}

impl From<Vec<(String, String)>> for Paragraph {
    fn from(v: Vec<(String, String)>) -> Self {
        v.into_iter().collect()
    }
}

impl From<Vec<(&str, &str)>> for Paragraph {
    fn from(v: Vec<(&str, &str)>) -> Self {
        v.into_iter().collect()
    }
}

impl FromIterator<(String, String)> for Paragraph {
    fn from_iter<T: IntoIterator<Item = (String, String)>>(iter: T) -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(PARAGRAPH.into());
        for (key, value) in iter {
            builder.start_node(ENTRY.into());
            builder.token(KEY.into(), &key);
            builder.token(COLON.into(), ":");
            builder.token(WHITESPACE.into(), " ");
            for (i, line) in value.split('\n').enumerate() {
                if i > 0 {
                    builder.token(INDENT.into(), " ");
                }
                builder.token(VALUE.into(), line);
                builder.token(NEWLINE.into(), "\n");
            }
            builder.finish_node();
        }
        builder.finish_node();
        Self(SyntaxNode::new_root_mut(builder.finish()))
    }
}

impl<'a> FromIterator<(&'a str, &'a str)> for Paragraph {
    fn from_iter<T: IntoIterator<Item = (&'a str, &'a str)>>(iter: T) -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(PARAGRAPH.into());
        for (key, value) in iter {
            builder.start_node(ENTRY.into());
            builder.token(KEY.into(), key);
            builder.token(COLON.into(), ":");
            builder.token(WHITESPACE.into(), " ");
            for (i, line) in value.split('\n').enumerate() {
                if i > 0 {
                    builder.token(INDENT.into(), " ");
                }
                builder.token(VALUE.into(), line);
                builder.token(NEWLINE.into(), "\n");
            }
            builder.finish_node();
        }
        builder.finish_node();
        Self(SyntaxNode::new_root_mut(builder.finish()))
    }
}

/// Detected indentation pattern for multi-line field values
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IndentPattern {
    /// All fields use a fixed number of spaces for indentation
    Fixed(usize),
    /// Each field's indentation matches its field name length + 2 (for ": ")
    FieldNameLength,
}

impl IndentPattern {
    /// Convert the pattern to a concrete indentation string for a given field name
    fn to_string(&self, field_name: &str) -> String {
        match self {
            IndentPattern::Fixed(spaces) => " ".repeat(*spaces),
            IndentPattern::FieldNameLength => " ".repeat(field_name.len() + 2),
        }
    }
}

impl Paragraph {
    /// Create a new empty paragraph.
    pub fn new() -> Paragraph {
        let mut builder = GreenNodeBuilder::new();

        builder.start_node(PARAGRAPH.into());
        builder.finish_node();
        Paragraph(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Reformat this paragraph
    ///
    /// # Arguments
    /// * `indentation` - The indentation to use
    /// * `immediate_empty_line` - Whether multi-line values should always start with an empty line
    /// * `max_line_length_one_liner` - If set, then this is the max length of the value if it is
    ///     crammed into a "one-liner" value
    /// * `sort_entries` - If set, then this function will sort the entries according to the given
    /// function
    /// * `format_value` - If set, then this function will format the value according to the given
    ///   function
    #[must_use]
    pub fn wrap_and_sort(
        &self,
        indentation: Indentation,
        immediate_empty_line: bool,
        max_line_length_one_liner: Option<usize>,
        sort_entries: Option<&dyn Fn(&Entry, &Entry) -> std::cmp::Ordering>,
        format_value: Option<&dyn Fn(&str, &str) -> String>,
    ) -> Paragraph {
        let mut builder = GreenNodeBuilder::new();

        let mut current = vec![];
        let mut entries = vec![];

        builder.start_node(PARAGRAPH.into());
        for c in self.0.children_with_tokens() {
            match c.kind() {
                ENTRY => {
                    entries.push((current, Entry::cast(c.as_node().unwrap().clone()).unwrap()));
                    current = vec![];
                }
                ERROR | COMMENT => {
                    current.push(c);
                }
                _ => {}
            }
        }

        if let Some(sort_entry) = sort_entries {
            entries.sort_by(|a, b| {
                let a_key = &a.1;
                let b_key = &b.1;
                sort_entry(a_key, b_key)
            });
        }

        for (pre, entry) in entries.into_iter() {
            for c in pre.into_iter() {
                builder.token(c.kind().into(), c.as_token().unwrap().text());
            }

            inject(
                &mut builder,
                entry
                    .wrap_and_sort(
                        indentation,
                        immediate_empty_line,
                        max_line_length_one_liner,
                        format_value,
                    )
                    .0,
            );
        }

        for c in current {
            builder.token(c.kind().into(), c.as_token().unwrap().text());
        }

        builder.finish_node();
        Self(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Normalize the spacing around field separators (colons) for all entries in place.
    ///
    /// This ensures that there is exactly one space after the colon and before the value
    /// for each field in the paragraph. This is a lossless operation that preserves the
    /// field names, values, and comments, but normalizes the whitespace formatting.
    ///
    /// # Examples
    ///
    /// ```
    /// use deb822_lossless::Deb822;
    /// use std::str::FromStr;
    ///
    /// let input = "Field1:    value1\nField2:value2\n";
    /// let mut deb822 = Deb822::from_str(input).unwrap();
    /// let mut para = deb822.paragraphs().next().unwrap();
    ///
    /// para.normalize_field_spacing();
    /// assert_eq!(para.to_string(), "Field1: value1\nField2: value2\n");
    /// ```
    pub fn normalize_field_spacing(&mut self) {
        use rowan::GreenNodeBuilder;

        // Collect entry information first to avoid borrowing issues
        let entries_to_normalize: Vec<_> = self
            .entries()
            .map(|entry| {
                let index = entry.0.index();

                // Build normalized entry inline
                let mut builder = GreenNodeBuilder::new();
                builder.start_node(ENTRY.into());

                let mut seen_colon = false;
                let mut skip_whitespace = false;

                for child in entry.0.children_with_tokens() {
                    match child.kind() {
                        KEY => {
                            builder.token(KEY.into(), child.as_token().unwrap().text());
                        }
                        COLON => {
                            builder.token(COLON.into(), ":");
                            seen_colon = true;
                            skip_whitespace = true;
                        }
                        WHITESPACE if skip_whitespace => {
                            // Skip existing whitespace after colon
                            continue;
                        }
                        VALUE if skip_whitespace => {
                            // Add exactly one space before the first value token
                            builder.token(WHITESPACE.into(), " ");
                            builder.token(VALUE.into(), child.as_token().unwrap().text());
                            skip_whitespace = false;
                        }
                        NEWLINE if skip_whitespace && seen_colon => {
                            // Empty value case (e.g., "Field:\n" or "Field:  \n")
                            // Normalize to no trailing space - just output newline
                            builder.token(NEWLINE.into(), "\n");
                            skip_whitespace = false;
                        }
                        _ => {
                            // Copy all other tokens as-is
                            if let Some(token) = child.as_token() {
                                builder.token(token.kind().into(), token.text());
                            }
                        }
                    }
                }

                builder.finish_node();
                (index, SyntaxNode::new_root_mut(builder.finish()))
            })
            .collect();

        // Now replace each entry with its normalized version
        for (index, normalized_entry) in entries_to_normalize {
            self.0
                .splice_children(index..index + 1, vec![normalized_entry.into()]);
        }
    }

    /// Returns the value of the given key in the paragraph.
    pub fn get(&self, key: &str) -> Option<String> {
        self.entries()
            .find(|e| e.key().as_deref() == Some(key))
            .map(|e| e.value())
    }

    /// Returns whether the paragraph contains the given key.
    pub fn contains_key(&self, key: &str) -> bool {
        self.get(key).is_some()
    }

    /// Returns an iterator over all entries in the paragraph.
    pub fn entries(&self) -> impl Iterator<Item = Entry> + '_ {
        self.0.children().filter_map(Entry::cast)
    }

    /// Returns an iterator over all items in the paragraph.
    pub fn items(&self) -> impl Iterator<Item = (String, String)> + '_ {
        self.entries()
            .filter_map(|e| e.key().map(|k| (k, e.value())))
    }

    /// Returns an iterator over all values for the given key in the paragraph.
    pub fn get_all<'a>(&'a self, key: &'a str) -> impl Iterator<Item = String> + 'a {
        self.items()
            .filter_map(move |(k, v)| if k == key { Some(v) } else { None })
    }

    /// Returns an iterator over all keys in the paragraph.
    pub fn keys(&self) -> impl Iterator<Item = String> + '_ {
        self.entries().filter_map(|e| e.key())
    }

    /// Remove the given field from the paragraph.
    pub fn remove(&mut self, key: &str) {
        for mut entry in self.entries() {
            if entry.key().as_deref() == Some(key) {
                entry.detach();
            }
        }
    }

    /// Insert a new field
    pub fn insert(&mut self, key: &str, value: &str) {
        let entry = Entry::new(key, value);
        let count = self.0.children_with_tokens().count();
        self.0.splice_children(count..count, vec![entry.0.into()]);
    }

    /// Insert a comment line before this paragraph.
    ///
    /// The comment should not include the leading '#' character or newline,
    /// these will be added automatically.
    ///
    /// # Examples
    ///
    /// ```
    /// use deb822_lossless::Deb822;
    /// let mut d: Deb822 = vec![
    ///     vec![("Foo", "Bar")].into_iter().collect(),
    /// ]
    /// .into_iter()
    /// .collect();
    /// let mut para = d.paragraphs().next().unwrap();
    /// para.insert_comment_before("This is a comment");
    /// assert_eq!(d.to_string(), "# This is a comment\nFoo: Bar\n");
    /// ```
    pub fn insert_comment_before(&mut self, comment: &str) {
        use rowan::GreenNodeBuilder;

        // Create an EMPTY_LINE node containing the comment tokens
        // This matches the structure used elsewhere in the parser
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(EMPTY_LINE.into());
        builder.token(COMMENT.into(), &format!("# {}", comment));
        builder.token(NEWLINE.into(), "\n");
        builder.finish_node();
        let green = builder.finish();

        // Convert to syntax node and insert before this paragraph
        let comment_node = SyntaxNode::new_root_mut(green);

        let index = self.0.index();
        let parent = self.0.parent().expect("Paragraph must have a parent");
        parent.splice_children(index..index, vec![comment_node.into()]);
    }

    /// Detect the indentation pattern used in this paragraph.
    ///
    /// This method analyzes existing multi-line fields to determine if they use:
    /// 1. A fixed indentation (all fields use the same number of spaces)
    /// 2. Field-name-length-based indentation (indent matches field name + ": ")
    ///
    /// If no pattern can be detected, defaults to field name length + 2.
    fn detect_indent_pattern(&self) -> IndentPattern {
        // Collect indentation data from existing multi-line fields
        let indent_data: Vec<(String, usize)> = self
            .entries()
            .filter_map(|entry| {
                let field_key = entry.key()?;
                let indent = entry.get_indent()?;
                Some((field_key, indent.len()))
            })
            .collect();

        if indent_data.is_empty() {
            // No existing multi-line fields, default to field name length
            return IndentPattern::FieldNameLength;
        }

        // Check if all fields use the same fixed indentation
        let first_indent_len = indent_data[0].1;
        let all_same = indent_data.iter().all(|(_, len)| *len == first_indent_len);

        if all_same {
            // All fields use the same indentation - use that
            return IndentPattern::Fixed(first_indent_len);
        }

        // Check if fields use field-name-length-based indentation
        let all_match_field_length = indent_data
            .iter()
            .all(|(field_key, indent_len)| *indent_len == field_key.len() + 2);

        if all_match_field_length {
            // Fields use field-name-length-based indentation
            return IndentPattern::FieldNameLength;
        }

        // Can't detect a clear pattern, default to field name length + 2
        IndentPattern::FieldNameLength
    }

    /// Set a field in the paragraph, inserting at the appropriate location if new
    pub fn set(&mut self, key: &str, value: &str) {
        self.set_with_indent_pattern(key, value, None, None);
    }

    /// Set a field using a specific field ordering
    pub fn set_with_field_order(&mut self, key: &str, value: &str, field_order: &[&str]) {
        self.set_with_indent_pattern(key, value, None, Some(field_order));
    }

    /// Set a field with optional default indentation pattern and field ordering.
    ///
    /// This method allows setting a field while optionally specifying a default indentation pattern
    /// to use when the field doesn't already have multi-line indentation to preserve.
    /// If the field already exists and is multi-line, its existing indentation is preserved.
    ///
    /// # Arguments
    /// * `key` - The field name
    /// * `value` - The field value
    /// * `default_indent_pattern` - Optional default indentation pattern to use for new fields or
    ///   fields without existing multi-line indentation. If None, will preserve existing field's
    ///   indentation or auto-detect from other fields
    /// * `field_order` - Optional field ordering for positioning the field. If None, inserts at end
    pub fn set_with_indent_pattern(
        &mut self,
        key: &str,
        value: &str,
        default_indent_pattern: Option<&IndentPattern>,
        field_order: Option<&[&str]>,
    ) {
        // Check if the field already exists and extract its formatting
        let existing_entry = self
            .entries()
            .find(|entry| entry.key().as_deref() == Some(key));

        // Determine indentation to use
        let indent = existing_entry
            .as_ref()
            .and_then(|entry| entry.get_indent())
            .unwrap_or_else(|| {
                // No existing indentation, use default pattern or auto-detect
                if let Some(pattern) = default_indent_pattern {
                    pattern.to_string(key)
                } else {
                    self.detect_indent_pattern().to_string(key)
                }
            });

        let post_colon_ws = existing_entry
            .as_ref()
            .and_then(|entry| entry.get_post_colon_whitespace())
            .unwrap_or_else(|| " ".to_string());

        let new_entry = Entry::with_formatting(key, value, &post_colon_ws, &indent);

        // Check if the field already exists and replace it
        for entry in self.entries() {
            if entry.key().as_deref() == Some(key) {
                self.0.splice_children(
                    entry.0.index()..entry.0.index() + 1,
                    vec![new_entry.0.into()],
                );
                return;
            }
        }

        // Insert new field
        if let Some(order) = field_order {
            let insertion_index = self.find_insertion_index(key, order);
            self.0
                .splice_children(insertion_index..insertion_index, vec![new_entry.0.into()]);
        } else {
            // Insert at the end if no field order specified
            let insertion_index = self.0.children_with_tokens().count();
            self.0
                .splice_children(insertion_index..insertion_index, vec![new_entry.0.into()]);
        }
    }

    /// Find the appropriate insertion index for a new field based on field ordering
    fn find_insertion_index(&self, key: &str, field_order: &[&str]) -> usize {
        // Find position of the new field in the canonical order
        let new_field_position = field_order.iter().position(|&field| field == key);

        let mut insertion_index = self.0.children_with_tokens().count();

        // Find the right position based on canonical field order
        for (i, child) in self.0.children_with_tokens().enumerate() {
            if let Some(node) = child.as_node() {
                if let Some(entry) = Entry::cast(node.clone()) {
                    if let Some(existing_key) = entry.key() {
                        let existing_position =
                            field_order.iter().position(|&field| field == existing_key);

                        match (new_field_position, existing_position) {
                            // Both fields are in the canonical order
                            (Some(new_pos), Some(existing_pos)) => {
                                if new_pos < existing_pos {
                                    insertion_index = i;
                                    break;
                                }
                            }
                            // New field is in canonical order, existing is not
                            (Some(_), None) => {
                                // Continue looking - unknown fields go after known ones
                            }
                            // New field is not in canonical order, existing is
                            (None, Some(_)) => {
                                // Continue until we find all known fields
                            }
                            // Neither field is in canonical order, maintain alphabetical
                            (None, None) => {
                                if key < existing_key.as_str() {
                                    insertion_index = i;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }

        // If we have a position in canonical order but haven't found where to insert yet,
        // we need to insert after all known fields that come before it
        if new_field_position.is_some() && insertion_index == self.0.children_with_tokens().count()
        {
            // Look for the position after the last known field that comes before our field
            let children: Vec<_> = self.0.children_with_tokens().enumerate().collect();
            for (i, child) in children.into_iter().rev() {
                if let Some(node) = child.as_node() {
                    if let Some(entry) = Entry::cast(node.clone()) {
                        if let Some(existing_key) = entry.key() {
                            if field_order
                                .iter()
                                .position(|&f| f == existing_key)
                                .is_some()
                            {
                                // Found a known field, insert after it
                                insertion_index = i + 1;
                                break;
                            }
                        }
                    }
                }
            }
        }

        insertion_index
    }

    /// Rename the given field in the paragraph.
    pub fn rename(&mut self, old_key: &str, new_key: &str) -> bool {
        for entry in self.entries() {
            if entry.key().as_deref() == Some(old_key) {
                self.0.splice_children(
                    entry.0.index()..entry.0.index() + 1,
                    vec![Entry::new(new_key, entry.value().as_str()).0.into()],
                );
                return true;
            }
        }
        false
    }
}

impl Default for Paragraph {
    fn default() -> Self {
        Self::new()
    }
}

impl std::str::FromStr for Paragraph {
    type Err = ParseError;

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        let deb822 = Deb822::from_str(text)?;

        let mut paragraphs = deb822.paragraphs();

        paragraphs
            .next()
            .ok_or_else(|| ParseError(vec!["no paragraphs".to_string()]))
    }
}

#[cfg(feature = "python-debian")]
impl<'py> pyo3::IntoPyObject<'py> for Paragraph {
    type Target = pyo3::PyAny;
    type Output = pyo3::Bound<'py, Self::Target>;
    type Error = pyo3::PyErr;

    fn into_pyobject(self, py: pyo3::Python<'py>) -> Result<Self::Output, Self::Error> {
        use pyo3::prelude::*;
        let d = pyo3::types::PyDict::new(py);
        for (k, v) in self.items() {
            d.set_item(k, v)?;
        }
        let m = py.import("debian.deb822")?;
        let cls = m.getattr("Deb822")?;
        cls.call1((d,))
    }
}

#[cfg(feature = "python-debian")]
impl<'a, 'py> pyo3::IntoPyObject<'py> for &'a Paragraph {
    type Target = pyo3::PyAny;
    type Output = pyo3::Bound<'py, Self::Target>;
    type Error = pyo3::PyErr;

    fn into_pyobject(self, py: pyo3::Python<'py>) -> Result<Self::Output, Self::Error> {
        use pyo3::prelude::*;
        let d = pyo3::types::PyDict::new(py);
        for (k, v) in self.items() {
            d.set_item(k, v)?;
        }
        let m = py.import("debian.deb822")?;
        let cls = m.getattr("Deb822")?;
        cls.call1((d,))
    }
}

#[cfg(feature = "python-debian")]
impl<'py> pyo3::FromPyObject<'_, 'py> for Paragraph {
    type Error = pyo3::PyErr;

    fn extract(obj: pyo3::Borrowed<'_, 'py, pyo3::PyAny>) -> Result<Self, Self::Error> {
        use pyo3::types::PyAnyMethods;
        let d = obj.call_method0("__str__")?.extract::<String>()?;
        Paragraph::from_str(&d)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err((e.to_string(),)))
    }
}

impl Entry {
    /// Returns the text range of this entry in the source text.
    pub fn text_range(&self) -> rowan::TextRange {
        self.0.text_range()
    }

    /// Returns the text range of the key (field name) in this entry.
    pub fn key_range(&self) -> Option<rowan::TextRange> {
        self.0
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find(|it| it.kind() == KEY)
            .map(|it| it.text_range())
    }

    /// Returns the text range of the colon separator in this entry.
    pub fn colon_range(&self) -> Option<rowan::TextRange> {
        self.0
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find(|it| it.kind() == COLON)
            .map(|it| it.text_range())
    }

    /// Returns the text range of the value portion (excluding the key and colon) in this entry.
    /// This includes all VALUE tokens and any continuation lines.
    pub fn value_range(&self) -> Option<rowan::TextRange> {
        let value_tokens: Vec<_> = self
            .0
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .filter(|it| it.kind() == VALUE)
            .collect();

        if value_tokens.is_empty() {
            return None;
        }

        let first = value_tokens.first().unwrap();
        let last = value_tokens.last().unwrap();
        Some(rowan::TextRange::new(
            first.text_range().start(),
            last.text_range().end(),
        ))
    }

    /// Returns the text ranges of all individual value lines in this entry.
    /// Multi-line values will return multiple ranges.
    pub fn value_line_ranges(&self) -> Vec<rowan::TextRange> {
        self.0
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .filter(|it| it.kind() == VALUE)
            .map(|it| it.text_range())
            .collect()
    }

    /// Create a new entry with the given key and value.
    pub fn new(key: &str, value: &str) -> Entry {
        Self::with_indentation(key, value, " ")
    }

    /// Create a new entry with the given key, value, and custom indentation for continuation lines.
    ///
    /// # Arguments
    /// * `key` - The field name
    /// * `value` - The field value (may contain '\n' for multi-line values)
    /// * `indent` - The indentation string to use for continuation lines
    pub fn with_indentation(key: &str, value: &str, indent: &str) -> Entry {
        Entry::with_formatting(key, value, " ", indent)
    }

    /// Create a new entry with specific formatting for post-colon whitespace and indentation.
    ///
    /// # Arguments
    /// * `key` - The field name
    /// * `value` - The field value (may contain '\n' for multi-line values)
    /// * `post_colon_ws` - The whitespace after the colon (e.g., " " or "\n ")
    /// * `indent` - The indentation string to use for continuation lines
    pub fn with_formatting(key: &str, value: &str, post_colon_ws: &str, indent: &str) -> Entry {
        let mut builder = GreenNodeBuilder::new();

        builder.start_node(ENTRY.into());
        builder.token(KEY.into(), key);
        builder.token(COLON.into(), ":");

        // Add the post-colon whitespace token by token
        let mut i = 0;
        while i < post_colon_ws.len() {
            if post_colon_ws[i..].starts_with('\n') {
                builder.token(NEWLINE.into(), "\n");
                i += 1;
            } else {
                // Collect consecutive non-newline chars as WHITESPACE
                let start = i;
                while i < post_colon_ws.len() && !post_colon_ws[i..].starts_with('\n') {
                    i += post_colon_ws[i..].chars().next().unwrap().len_utf8();
                }
                builder.token(WHITESPACE.into(), &post_colon_ws[start..i]);
            }
        }

        for (line_idx, line) in value.split('\n').enumerate() {
            if line_idx > 0 {
                builder.token(INDENT.into(), indent);
            }
            builder.token(VALUE.into(), line);
            builder.token(NEWLINE.into(), "\n");
        }
        builder.finish_node();
        Entry(SyntaxNode::new_root_mut(builder.finish()))
    }

    #[must_use]
    /// Reformat this entry
    ///
    /// # Arguments
    /// * `indentation` - The indentation to use
    /// * `immediate_empty_line` - Whether multi-line values should always start with an empty line
    /// * `max_line_length_one_liner` - If set, then this is the max length of the value if it is
    ///    crammed into a "one-liner" value
    /// * `format_value` - If set, then this function will format the value according to the given
    ///    function
    ///
    /// # Returns
    /// The reformatted entry
    pub fn wrap_and_sort(
        &self,
        mut indentation: Indentation,
        immediate_empty_line: bool,
        max_line_length_one_liner: Option<usize>,
        format_value: Option<&dyn Fn(&str, &str) -> String>,
    ) -> Entry {
        let mut builder = GreenNodeBuilder::new();

        let mut content = vec![];
        builder.start_node(ENTRY.into());
        for c in self.0.children_with_tokens() {
            let text = c.as_token().map(|t| t.text());
            match c.kind() {
                KEY => {
                    builder.token(KEY.into(), text.unwrap());
                    if indentation == Indentation::FieldNameLength {
                        indentation = Indentation::Spaces(text.unwrap().len() as u32);
                    }
                }
                COLON => {
                    builder.token(COLON.into(), ":");
                }
                INDENT => {
                    // Discard original whitespace
                }
                ERROR | COMMENT | VALUE | WHITESPACE | NEWLINE => {
                    content.push(c);
                }
                EMPTY_LINE | ENTRY | ROOT | PARAGRAPH => unreachable!(),
            }
        }

        let indentation = if let crate::Indentation::Spaces(i) = indentation {
            i
        } else {
            1
        };

        assert!(indentation > 0);

        // Strip trailing whitespace and newlines
        while let Some(c) = content.last() {
            if c.kind() == NEWLINE || c.kind() == WHITESPACE {
                content.pop();
            } else {
                break;
            }
        }

        // Reformat iff there is a format function and the value
        // has no errors or comments
        let tokens = if let Some(ref format_value) = format_value {
            if !content
                .iter()
                .any(|c| c.kind() == ERROR || c.kind() == COMMENT)
            {
                let concat = content
                    .iter()
                    .filter_map(|c| c.as_token().map(|t| t.text()))
                    .collect::<String>();
                let formatted = format_value(self.key().as_ref().unwrap(), &concat);
                crate::lex::lex_inline(&formatted)
                    .map(|(k, t)| (k, t.to_string()))
                    .collect::<Vec<_>>()
            } else {
                content
                    .into_iter()
                    .map(|n| n.into_token().unwrap())
                    .map(|i| (i.kind(), i.text().to_string()))
                    .collect::<Vec<_>>()
            }
        } else {
            content
                .into_iter()
                .map(|n| n.into_token().unwrap())
                .map(|i| (i.kind(), i.text().to_string()))
                .collect::<Vec<_>>()
        };

        rebuild_value(
            &mut builder,
            tokens,
            self.key().map_or(0, |k| k.len()),
            indentation,
            immediate_empty_line,
            max_line_length_one_liner,
        );

        builder.finish_node();
        Self(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Returns the key of the entry.
    pub fn key(&self) -> Option<String> {
        self.0
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find(|it| it.kind() == KEY)
            .map(|it| it.text().to_string())
    }

    /// Returns the value of the entry.
    pub fn value(&self) -> String {
        let mut parts = self
            .0
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .filter(|it| it.kind() == VALUE)
            .map(|it| it.text().to_string());

        match parts.next() {
            None => String::new(),
            Some(first) => {
                let mut result = first;
                for part in parts {
                    result.push('\n');
                    result.push_str(&part);
                }
                result
            }
        }
    }

    /// Returns the indentation string used for continuation lines in this entry.
    /// Returns None if the entry has no continuation lines.
    fn get_indent(&self) -> Option<String> {
        self.0
            .children_with_tokens()
            .filter_map(|it| it.into_token())
            .find(|it| it.kind() == INDENT)
            .map(|it| it.text().to_string())
    }

    /// Returns the whitespace immediately after the colon in this entry.
    /// This includes WHITESPACE, NEWLINE, and INDENT tokens up to the first VALUE token.
    /// Returns None if there is no whitespace (which would be malformed).
    fn get_post_colon_whitespace(&self) -> Option<String> {
        let mut found_colon = false;
        let mut whitespace = String::new();

        for token in self
            .0
            .children_with_tokens()
            .filter_map(|it| it.into_token())
        {
            if token.kind() == COLON {
                found_colon = true;
                continue;
            }

            if found_colon {
                if token.kind() == WHITESPACE || token.kind() == NEWLINE || token.kind() == INDENT {
                    whitespace.push_str(token.text());
                } else {
                    // We've reached a non-whitespace token, stop collecting
                    break;
                }
            }
        }

        if whitespace.is_empty() {
            None
        } else {
            Some(whitespace)
        }
    }

    /// Normalize the spacing around the field separator (colon) in place.
    ///
    /// This ensures that there is exactly one space after the colon and before the value.
    /// This is a lossless operation that preserves the field name and value content,
    /// but normalizes the whitespace formatting.
    ///
    /// # Examples
    ///
    /// ```
    /// use deb822_lossless::Deb822;
    /// use std::str::FromStr;
    ///
    /// // Parse an entry with extra spacing after the colon
    /// let input = "Field:    value\n";
    /// let mut deb822 = Deb822::from_str(input).unwrap();
    /// let mut para = deb822.paragraphs().next().unwrap();
    ///
    /// para.normalize_field_spacing();
    /// assert_eq!(para.get("Field").as_deref(), Some("value"));
    /// ```
    pub fn normalize_field_spacing(&mut self) {
        use rowan::GreenNodeBuilder;

        // Build normalized entry
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(ENTRY.into());

        let mut seen_colon = false;
        let mut skip_whitespace = false;

        for child in self.0.children_with_tokens() {
            match child.kind() {
                KEY => {
                    builder.token(KEY.into(), child.as_token().unwrap().text());
                }
                COLON => {
                    builder.token(COLON.into(), ":");
                    seen_colon = true;
                    skip_whitespace = true;
                }
                WHITESPACE if skip_whitespace => {
                    // Skip existing whitespace after colon
                    continue;
                }
                VALUE if skip_whitespace => {
                    // Add exactly one space before the first value token
                    builder.token(WHITESPACE.into(), " ");
                    builder.token(VALUE.into(), child.as_token().unwrap().text());
                    skip_whitespace = false;
                }
                NEWLINE if skip_whitespace && seen_colon => {
                    // Empty value case (e.g., "Field:\n" or "Field:  \n")
                    // Normalize to no trailing space - just output newline
                    builder.token(NEWLINE.into(), "\n");
                    skip_whitespace = false;
                }
                _ => {
                    // Copy all other tokens as-is
                    if let Some(token) = child.as_token() {
                        builder.token(token.kind().into(), token.text());
                    }
                }
            }
        }

        builder.finish_node();
        let normalized = SyntaxNode::new_root_mut(builder.finish());

        // Replace this entry in place
        if let Some(parent) = self.0.parent() {
            let index = self.0.index();
            parent.splice_children(index..index + 1, vec![normalized.into()]);
        }
    }

    /// Detach this entry from the paragraph.
    pub fn detach(&mut self) {
        self.0.detach();
    }
}

impl FromStr for Deb822 {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Deb822::parse(s).to_result()
    }
}

#[test]
fn test_parse_simple() {
    const CONTROLV1: &str = r#"Source: foo
Maintainer: Foo Bar <foo@example.com>
Section: net

# This is a comment

Package: foo
Architecture: all
Depends:
 bar,
 blah
Description: This is a description
 And it is
 .
 multiple
 lines
"#;
    let parsed = parse(CONTROLV1);
    let node = parsed.syntax();
    assert_eq!(
        format!("{:#?}", node),
        r###"ROOT@0..203
  PARAGRAPH@0..63
    ENTRY@0..12
      KEY@0..6 "Source"
      COLON@6..7 ":"
      WHITESPACE@7..8 " "
      VALUE@8..11 "foo"
      NEWLINE@11..12 "\n"
    ENTRY@12..50
      KEY@12..22 "Maintainer"
      COLON@22..23 ":"
      WHITESPACE@23..24 " "
      VALUE@24..49 "Foo Bar <foo@example. ..."
      NEWLINE@49..50 "\n"
    ENTRY@50..63
      KEY@50..57 "Section"
      COLON@57..58 ":"
      WHITESPACE@58..59 " "
      VALUE@59..62 "net"
      NEWLINE@62..63 "\n"
  EMPTY_LINE@63..64
    NEWLINE@63..64 "\n"
  EMPTY_LINE@64..84
    COMMENT@64..83 "# This is a comment"
    NEWLINE@83..84 "\n"
  EMPTY_LINE@84..85
    NEWLINE@84..85 "\n"
  PARAGRAPH@85..203
    ENTRY@85..98
      KEY@85..92 "Package"
      COLON@92..93 ":"
      WHITESPACE@93..94 " "
      VALUE@94..97 "foo"
      NEWLINE@97..98 "\n"
    ENTRY@98..116
      KEY@98..110 "Architecture"
      COLON@110..111 ":"
      WHITESPACE@111..112 " "
      VALUE@112..115 "all"
      NEWLINE@115..116 "\n"
    ENTRY@116..137
      KEY@116..123 "Depends"
      COLON@123..124 ":"
      NEWLINE@124..125 "\n"
      INDENT@125..126 " "
      VALUE@126..130 "bar,"
      NEWLINE@130..131 "\n"
      INDENT@131..132 " "
      VALUE@132..136 "blah"
      NEWLINE@136..137 "\n"
    ENTRY@137..203
      KEY@137..148 "Description"
      COLON@148..149 ":"
      WHITESPACE@149..150 " "
      VALUE@150..171 "This is a description"
      NEWLINE@171..172 "\n"
      INDENT@172..173 " "
      VALUE@173..182 "And it is"
      NEWLINE@182..183 "\n"
      INDENT@183..184 " "
      VALUE@184..185 "."
      NEWLINE@185..186 "\n"
      INDENT@186..187 " "
      VALUE@187..195 "multiple"
      NEWLINE@195..196 "\n"
      INDENT@196..197 " "
      VALUE@197..202 "lines"
      NEWLINE@202..203 "\n"
"###
    );
    assert_eq!(parsed.errors, Vec::<String>::new());

    let root = parsed.root_mut();
    assert_eq!(root.paragraphs().count(), 2);
    let source = root.paragraphs().next().unwrap();
    assert_eq!(
        source.keys().collect::<Vec<_>>(),
        vec!["Source", "Maintainer", "Section"]
    );
    assert_eq!(source.get("Source").as_deref(), Some("foo"));
    assert_eq!(
        source.get("Maintainer").as_deref(),
        Some("Foo Bar <foo@example.com>")
    );
    assert_eq!(source.get("Section").as_deref(), Some("net"));
    assert_eq!(
        source.items().collect::<Vec<_>>(),
        vec![
            ("Source".into(), "foo".into()),
            ("Maintainer".into(), "Foo Bar <foo@example.com>".into()),
            ("Section".into(), "net".into()),
        ]
    );

    let binary = root.paragraphs().nth(1).unwrap();
    assert_eq!(
        binary.keys().collect::<Vec<_>>(),
        vec!["Package", "Architecture", "Depends", "Description"]
    );
    assert_eq!(binary.get("Package").as_deref(), Some("foo"));
    assert_eq!(binary.get("Architecture").as_deref(), Some("all"));
    assert_eq!(binary.get("Depends").as_deref(), Some("bar,\nblah"));
    assert_eq!(
        binary.get("Description").as_deref(),
        Some("This is a description\nAnd it is\n.\nmultiple\nlines")
    );

    assert_eq!(node.text(), CONTROLV1);
}

#[test]
fn test_with_trailing_whitespace() {
    const CONTROLV1: &str = r#"Source: foo
Maintainer: Foo Bar <foo@example.com>


"#;
    let parsed = parse(CONTROLV1);
    let node = parsed.syntax();
    assert_eq!(
        format!("{:#?}", node),
        r###"ROOT@0..52
  PARAGRAPH@0..50
    ENTRY@0..12
      KEY@0..6 "Source"
      COLON@6..7 ":"
      WHITESPACE@7..8 " "
      VALUE@8..11 "foo"
      NEWLINE@11..12 "\n"
    ENTRY@12..50
      KEY@12..22 "Maintainer"
      COLON@22..23 ":"
      WHITESPACE@23..24 " "
      VALUE@24..49 "Foo Bar <foo@example. ..."
      NEWLINE@49..50 "\n"
  EMPTY_LINE@50..51
    NEWLINE@50..51 "\n"
  EMPTY_LINE@51..52
    NEWLINE@51..52 "\n"
"###
    );
    assert_eq!(parsed.errors, Vec::<String>::new());

    let root = parsed.root_mut();
    assert_eq!(root.paragraphs().count(), 1);
    let source = root.paragraphs().next().unwrap();
    assert_eq!(
        source.items().collect::<Vec<_>>(),
        vec![
            ("Source".into(), "foo".into()),
            ("Maintainer".into(), "Foo Bar <foo@example.com>".into()),
        ]
    );
}

fn rebuild_value(
    builder: &mut GreenNodeBuilder,
    mut tokens: Vec<(SyntaxKind, String)>,
    key_len: usize,
    indentation: u32,
    immediate_empty_line: bool,
    max_line_length_one_liner: Option<usize>,
) {
    let first_line_len = tokens
        .iter()
        .take_while(|(k, _t)| *k != NEWLINE)
        .map(|(_k, t)| t.len())
        .sum::<usize>() + key_len + 2 /* ": " */;

    let has_newline = tokens.iter().any(|(k, _t)| *k == NEWLINE);

    let mut last_was_newline = false;
    if max_line_length_one_liner
        .map(|mll| first_line_len <= mll)
        .unwrap_or(false)
        && !has_newline
    {
        // Just copy tokens if the value fits into one line
        for (k, t) in tokens {
            builder.token(k.into(), &t);
        }
    } else {
        // Insert a leading newline if the value is multi-line and immediate_empty_line is set
        if immediate_empty_line && has_newline {
            builder.token(NEWLINE.into(), "\n");
            last_was_newline = true;
        } else {
            builder.token(WHITESPACE.into(), " ");
        }
        // Strip leading whitespace and newlines
        let mut start_idx = 0;
        while start_idx < tokens.len() {
            if tokens[start_idx].0 == NEWLINE || tokens[start_idx].0 == WHITESPACE {
                start_idx += 1;
            } else {
                break;
            }
        }
        tokens.drain(..start_idx);
        // Pre-allocate indentation string to avoid repeated allocations
        let indent_str = " ".repeat(indentation as usize);
        for (k, t) in tokens {
            if last_was_newline {
                builder.token(INDENT.into(), &indent_str);
            }
            builder.token(k.into(), &t);
            last_was_newline = k == NEWLINE;
        }
    }

    if !last_was_newline {
        builder.token(NEWLINE.into(), "\n");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_parse() {
        let d: super::Deb822 = r#"Source: foo
Maintainer: Foo Bar <jelmer@jelmer.uk>
Section: net

Package: foo
Architecture: all
Depends: libc6
Description: This is a description
 With details
 "#
        .parse()
        .unwrap();
        let mut ps = d.paragraphs();
        let p = ps.next().unwrap();

        assert_eq!(p.get("Source").as_deref(), Some("foo"));
        assert_eq!(
            p.get("Maintainer").as_deref(),
            Some("Foo Bar <jelmer@jelmer.uk>")
        );
        assert_eq!(p.get("Section").as_deref(), Some("net"));

        let b = ps.next().unwrap();
        assert_eq!(b.get("Package").as_deref(), Some("foo"));
    }

    #[test]
    fn test_after_multi_line() {
        let d: super::Deb822 = r#"Source: golang-github-blah-blah
Section: devel
Priority: optional
Standards-Version: 4.2.0
Maintainer: Some Maintainer <example@example.com>
Build-Depends: debhelper (>= 11~),
               dh-golang,
               golang-any
Homepage: https://github.com/j-keck/arping
"#
        .parse()
        .unwrap();
        let mut ps = d.paragraphs();
        let p = ps.next().unwrap();
        assert_eq!(p.get("Source").as_deref(), Some("golang-github-blah-blah"));
        assert_eq!(p.get("Section").as_deref(), Some("devel"));
        assert_eq!(p.get("Priority").as_deref(), Some("optional"));
        assert_eq!(p.get("Standards-Version").as_deref(), Some("4.2.0"));
        assert_eq!(
            p.get("Maintainer").as_deref(),
            Some("Some Maintainer <example@example.com>")
        );
        assert_eq!(
            p.get("Build-Depends").as_deref(),
            Some("debhelper (>= 11~),\ndh-golang,\ngolang-any")
        );
        assert_eq!(
            p.get("Homepage").as_deref(),
            Some("https://github.com/j-keck/arping")
        );
    }

    #[test]
    fn test_remove_field() {
        let d: super::Deb822 = r#"Source: foo
# Comment
Maintainer: Foo Bar <jelmer@jelmer.uk>
Section: net

Package: foo
Architecture: all
Depends: libc6
Description: This is a description
 With details
 "#
        .parse()
        .unwrap();
        let mut ps = d.paragraphs();
        let mut p = ps.next().unwrap();
        p.set("Foo", "Bar");
        p.remove("Section");
        p.remove("Nonexistent");
        assert_eq!(p.get("Foo").as_deref(), Some("Bar"));
        assert_eq!(
            p.to_string(),
            r#"Source: foo
# Comment
Maintainer: Foo Bar <jelmer@jelmer.uk>
Foo: Bar
"#
        );
    }

    #[test]
    fn test_rename_field() {
        let d: super::Deb822 = r#"Source: foo
Vcs-Browser: https://salsa.debian.org/debian/foo
"#
        .parse()
        .unwrap();
        let mut ps = d.paragraphs();
        let mut p = ps.next().unwrap();
        assert!(p.rename("Vcs-Browser", "Homepage"));
        assert_eq!(
            p.to_string(),
            r#"Source: foo
Homepage: https://salsa.debian.org/debian/foo
"#
        );

        assert_eq!(
            p.get("Homepage").as_deref(),
            Some("https://salsa.debian.org/debian/foo")
        );
        assert_eq!(p.get("Vcs-Browser").as_deref(), None);

        // Nonexistent field
        assert!(!p.rename("Nonexistent", "Homepage"));
    }

    #[test]
    fn test_set_field() {
        let d: super::Deb822 = r#"Source: foo
Maintainer: Foo Bar <joe@example.com>
"#
        .parse()
        .unwrap();
        let mut ps = d.paragraphs();
        let mut p = ps.next().unwrap();
        p.set("Maintainer", "Somebody Else <jane@example.com>");
        assert_eq!(
            p.get("Maintainer").as_deref(),
            Some("Somebody Else <jane@example.com>")
        );
        assert_eq!(
            p.to_string(),
            r#"Source: foo
Maintainer: Somebody Else <jane@example.com>
"#
        );
    }

    #[test]
    fn test_set_new_field() {
        let d: super::Deb822 = r#"Source: foo
"#
        .parse()
        .unwrap();
        let mut ps = d.paragraphs();
        let mut p = ps.next().unwrap();
        p.set("Maintainer", "Somebody <joe@example.com>");
        assert_eq!(
            p.get("Maintainer").as_deref(),
            Some("Somebody <joe@example.com>")
        );
        assert_eq!(
            p.to_string(),
            r#"Source: foo
Maintainer: Somebody <joe@example.com>
"#
        );
    }

    #[test]
    fn test_add_paragraph() {
        let mut d = super::Deb822::new();
        let mut p = d.add_paragraph();
        p.set("Foo", "Bar");
        assert_eq!(p.get("Foo").as_deref(), Some("Bar"));
        assert_eq!(
            p.to_string(),
            r#"Foo: Bar
"#
        );
        assert_eq!(
            d.to_string(),
            r#"Foo: Bar
"#
        );

        let mut p = d.add_paragraph();
        p.set("Foo", "Blah");
        assert_eq!(p.get("Foo").as_deref(), Some("Blah"));
        assert_eq!(
            d.to_string(),
            r#"Foo: Bar

Foo: Blah
"#
        );
    }

    #[test]
    fn test_crud_paragraph() {
        let mut d = super::Deb822::new();
        let mut p = d.insert_paragraph(0);
        p.set("Foo", "Bar");
        assert_eq!(p.get("Foo").as_deref(), Some("Bar"));
        assert_eq!(
            d.to_string(),
            r#"Foo: Bar
"#
        );

        // test prepend
        let mut p = d.insert_paragraph(0);
        p.set("Foo", "Blah");
        assert_eq!(p.get("Foo").as_deref(), Some("Blah"));
        assert_eq!(
            d.to_string(),
            r#"Foo: Blah

Foo: Bar
"#
        );

        // test delete
        d.remove_paragraph(1);
        assert_eq!(d.to_string(), "Foo: Blah\n\n");

        // test update again
        p.set("Foo", "Baz");
        assert_eq!(d.to_string(), "Foo: Baz\n\n");

        // test delete again
        d.remove_paragraph(0);
        assert_eq!(d.to_string(), "");
    }

    #[test]
    fn test_swap_paragraphs() {
        // Test basic swap
        let mut d: super::Deb822 = vec![
            vec![("Foo", "Bar")].into_iter().collect(),
            vec![("A", "B")].into_iter().collect(),
            vec![("X", "Y")].into_iter().collect(),
        ]
        .into_iter()
        .collect();

        d.swap_paragraphs(0, 2);
        assert_eq!(d.to_string(), "X: Y\n\nA: B\n\nFoo: Bar\n");

        // Swap back
        d.swap_paragraphs(0, 2);
        assert_eq!(d.to_string(), "Foo: Bar\n\nA: B\n\nX: Y\n");

        // Swap adjacent paragraphs
        d.swap_paragraphs(0, 1);
        assert_eq!(d.to_string(), "A: B\n\nFoo: Bar\n\nX: Y\n");

        // Swap with same index should be no-op
        let before = d.to_string();
        d.swap_paragraphs(1, 1);
        assert_eq!(d.to_string(), before);
    }

    #[test]
    fn test_swap_paragraphs_preserves_content() {
        // Test that field content is preserved
        let mut d: super::Deb822 = vec![
            vec![("Field1", "Value1"), ("Field2", "Value2")]
                .into_iter()
                .collect(),
            vec![("FieldA", "ValueA"), ("FieldB", "ValueB")]
                .into_iter()
                .collect(),
        ]
        .into_iter()
        .collect();

        d.swap_paragraphs(0, 1);

        let mut paras = d.paragraphs();
        let p1 = paras.next().unwrap();
        assert_eq!(p1.get("FieldA").as_deref(), Some("ValueA"));
        assert_eq!(p1.get("FieldB").as_deref(), Some("ValueB"));

        let p2 = paras.next().unwrap();
        assert_eq!(p2.get("Field1").as_deref(), Some("Value1"));
        assert_eq!(p2.get("Field2").as_deref(), Some("Value2"));
    }

    #[test]
    #[should_panic(expected = "out of bounds")]
    fn test_swap_paragraphs_out_of_bounds() {
        let mut d: super::Deb822 = vec![
            vec![("Foo", "Bar")].into_iter().collect(),
            vec![("A", "B")].into_iter().collect(),
        ]
        .into_iter()
        .collect();

        d.swap_paragraphs(0, 5);
    }

    #[test]
    fn test_multiline_entry() {
        use super::SyntaxKind::*;
        use rowan::ast::AstNode;

        let entry = super::Entry::new("foo", "bar\nbaz");
        let tokens: Vec<_> = entry
            .syntax()
            .descendants_with_tokens()
            .filter_map(|tok| tok.into_token())
            .collect();

        assert_eq!("foo: bar\n baz\n", entry.to_string());
        assert_eq!("bar\nbaz", entry.value());

        assert_eq!(
            vec![
                (KEY, "foo"),
                (COLON, ":"),
                (WHITESPACE, " "),
                (VALUE, "bar"),
                (NEWLINE, "\n"),
                (INDENT, " "),
                (VALUE, "baz"),
                (NEWLINE, "\n"),
            ],
            tokens
                .iter()
                .map(|token| (token.kind(), token.text()))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_apt_entry() {
        let text = r#"Package: cvsd
Binary: cvsd
Version: 1.0.24
Maintainer: Arthur de Jong <adejong@debian.org>
Build-Depends: debhelper (>= 9), po-debconf
Architecture: any
Standards-Version: 3.9.3
Format: 3.0 (native)
Files:
 b7a7d67a02974c52c408fdb5e118406d 890 cvsd_1.0.24.dsc
 b73ee40774c3086cb8490cdbb96ac883 258139 cvsd_1.0.24.tar.gz
Vcs-Browser: http://arthurdejong.org/viewvc/cvsd/
Vcs-Cvs: :pserver:anonymous@arthurdejong.org:/arthur/
Checksums-Sha256:
 a7bb7a3aacee19cd14ce5c26cb86e348b1608e6f1f6e97c6ea7c58efa440ac43 890 cvsd_1.0.24.dsc
 46bc517760c1070ae408693b89603986b53e6f068ae6bdc744e2e830e46b8cba 258139 cvsd_1.0.24.tar.gz
Homepage: http://arthurdejong.org/cvsd/
Package-List:
 cvsd deb vcs optional
Directory: pool/main/c/cvsd
Priority: source
Section: vcs

"#;
        let d: super::Deb822 = text.parse().unwrap();
        let p = d.paragraphs().next().unwrap();
        assert_eq!(p.get("Binary").as_deref(), Some("cvsd"));
        assert_eq!(p.get("Version").as_deref(), Some("1.0.24"));
        assert_eq!(
            p.get("Maintainer").as_deref(),
            Some("Arthur de Jong <adejong@debian.org>")
        );
    }

    #[test]
    fn test_format() {
        let d: super::Deb822 = r#"Source: foo
Maintainer: Foo Bar <foo@example.com>
Section:      net
Blah: blah  # comment
Multi-Line:
  Ahoi!
     Matey!

"#
        .parse()
        .unwrap();
        let mut ps = d.paragraphs();
        let p = ps.next().unwrap();
        let result = p.wrap_and_sort(
            crate::Indentation::FieldNameLength,
            false,
            None,
            None::<&dyn Fn(&super::Entry, &super::Entry) -> std::cmp::Ordering>,
            None,
        );
        assert_eq!(
            result.to_string(),
            r#"Source: foo
Maintainer: Foo Bar <foo@example.com>
Section: net
Blah: blah  # comment
Multi-Line: Ahoi!
          Matey!
"#
        );
    }

    #[test]
    fn test_format_sort_paragraphs() {
        let d: super::Deb822 = r#"Source: foo
Maintainer: Foo Bar <foo@example.com>

# This is a comment
Source: bar
Maintainer: Bar Foo <bar@example.com>

"#
        .parse()
        .unwrap();
        let result = d.wrap_and_sort(
            Some(&|a: &super::Paragraph, b: &super::Paragraph| {
                a.get("Source").cmp(&b.get("Source"))
            }),
            Some(&|p| {
                p.wrap_and_sort(
                    crate::Indentation::FieldNameLength,
                    false,
                    None,
                    None::<&dyn Fn(&super::Entry, &super::Entry) -> std::cmp::Ordering>,
                    None,
                )
            }),
        );
        assert_eq!(
            result.to_string(),
            r#"# This is a comment
Source: bar
Maintainer: Bar Foo <bar@example.com>

Source: foo
Maintainer: Foo Bar <foo@example.com>
"#,
        );
    }

    #[test]
    fn test_format_sort_fields() {
        let d: super::Deb822 = r#"Source: foo
Maintainer: Foo Bar <foo@example.com>
Build-Depends: debhelper (>= 9), po-debconf
Homepage: https://example.com/

"#
        .parse()
        .unwrap();
        let result = d.wrap_and_sort(
            None,
            Some(&|p: &super::Paragraph| -> super::Paragraph {
                p.wrap_and_sort(
                    crate::Indentation::FieldNameLength,
                    false,
                    None,
                    Some(&|a: &super::Entry, b: &super::Entry| a.key().cmp(&b.key())),
                    None,
                )
            }),
        );
        assert_eq!(
            result.to_string(),
            r#"Build-Depends: debhelper (>= 9), po-debconf
Homepage: https://example.com/
Maintainer: Foo Bar <foo@example.com>
Source: foo
"#
        );
    }

    #[test]
    fn test_para_from_iter() {
        let p: super::Paragraph = vec![("Foo", "Bar"), ("Baz", "Qux")].into_iter().collect();
        assert_eq!(
            p.to_string(),
            r#"Foo: Bar
Baz: Qux
"#
        );

        let p: super::Paragraph = vec![
            ("Foo".to_string(), "Bar".to_string()),
            ("Baz".to_string(), "Qux".to_string()),
        ]
        .into_iter()
        .collect();

        assert_eq!(
            p.to_string(),
            r#"Foo: Bar
Baz: Qux
"#
        );
    }

    #[test]
    fn test_deb822_from_iter() {
        let d: super::Deb822 = vec![
            vec![("Foo", "Bar"), ("Baz", "Qux")].into_iter().collect(),
            vec![("A", "B"), ("C", "D")].into_iter().collect(),
        ]
        .into_iter()
        .collect();
        assert_eq!(
            d.to_string(),
            r#"Foo: Bar
Baz: Qux

A: B
C: D
"#
        );
    }

    #[test]
    fn test_format_parse_error() {
        assert_eq!(ParseError(vec!["foo".to_string()]).to_string(), "foo\n");
    }

    #[test]
    fn test_set_with_field_order() {
        let mut p = super::Paragraph::new();
        let custom_order = &["Foo", "Bar", "Baz"];

        p.set_with_field_order("Baz", "3", custom_order);
        p.set_with_field_order("Foo", "1", custom_order);
        p.set_with_field_order("Bar", "2", custom_order);
        p.set_with_field_order("Unknown", "4", custom_order);

        let keys: Vec<_> = p.keys().collect();
        assert_eq!(keys[0], "Foo");
        assert_eq!(keys[1], "Bar");
        assert_eq!(keys[2], "Baz");
        assert_eq!(keys[3], "Unknown");
    }

    #[test]
    fn test_positioned_parse_error() {
        let error = PositionedParseError {
            message: "test error".to_string(),
            range: rowan::TextRange::new(rowan::TextSize::from(5), rowan::TextSize::from(10)),
            code: Some("test_code".to_string()),
        };
        assert_eq!(error.to_string(), "test error");
        assert_eq!(error.range.start(), rowan::TextSize::from(5));
        assert_eq!(error.range.end(), rowan::TextSize::from(10));
        assert_eq!(error.code, Some("test_code".to_string()));
    }

    #[test]
    fn test_format_error() {
        assert_eq!(
            super::Error::ParseError(ParseError(vec!["foo".to_string()])).to_string(),
            "foo\n"
        );
    }

    #[test]
    fn test_get_all() {
        let d: super::Deb822 = r#"Source: foo
Maintainer: Foo Bar <foo@example.com>
Maintainer: Bar Foo <bar@example.com>"#
            .parse()
            .unwrap();
        let p = d.paragraphs().next().unwrap();
        assert_eq!(
            p.get_all("Maintainer").collect::<Vec<_>>(),
            vec!["Foo Bar <foo@example.com>", "Bar Foo <bar@example.com>"]
        );
    }

    #[test]
    fn test_entry_ranges() {
        let input = r#"Package: test-package
Maintainer: Test User <test@example.com>
Description: A simple test package
 with multiple lines
 of description text"#;

        let deb822 = super::Deb822::from_str(input).unwrap();
        let paragraph = deb822.paragraphs().next().unwrap();
        let entries: Vec<_> = paragraph.entries().collect();

        // Test first entry (Package)
        let package_entry = &entries[0];
        assert_eq!(package_entry.key(), Some("Package".to_string()));

        // Test key_range
        let key_range = package_entry.key_range().unwrap();
        assert_eq!(
            &input[key_range.start().into()..key_range.end().into()],
            "Package"
        );

        // Test colon_range
        let colon_range = package_entry.colon_range().unwrap();
        assert_eq!(
            &input[colon_range.start().into()..colon_range.end().into()],
            ":"
        );

        // Test value_range
        let value_range = package_entry.value_range().unwrap();
        assert_eq!(
            &input[value_range.start().into()..value_range.end().into()],
            "test-package"
        );

        // Test text_range covers the whole entry
        let text_range = package_entry.text_range();
        assert_eq!(
            &input[text_range.start().into()..text_range.end().into()],
            "Package: test-package\n"
        );

        // Test single-line value_line_ranges
        let value_lines = package_entry.value_line_ranges();
        assert_eq!(value_lines.len(), 1);
        assert_eq!(
            &input[value_lines[0].start().into()..value_lines[0].end().into()],
            "test-package"
        );
    }

    #[test]
    fn test_multiline_entry_ranges() {
        let input = r#"Description: Short description
 Extended description line 1
 Extended description line 2"#;

        let deb822 = super::Deb822::from_str(input).unwrap();
        let paragraph = deb822.paragraphs().next().unwrap();
        let entry = paragraph.entries().next().unwrap();

        assert_eq!(entry.key(), Some("Description".to_string()));

        // Test value_range spans all lines
        let value_range = entry.value_range().unwrap();
        let full_value = &input[value_range.start().into()..value_range.end().into()];
        assert!(full_value.contains("Short description"));
        assert!(full_value.contains("Extended description line 1"));
        assert!(full_value.contains("Extended description line 2"));

        // Test value_line_ranges gives individual lines
        let value_lines = entry.value_line_ranges();
        assert_eq!(value_lines.len(), 3);

        assert_eq!(
            &input[value_lines[0].start().into()..value_lines[0].end().into()],
            "Short description"
        );
        assert_eq!(
            &input[value_lines[1].start().into()..value_lines[1].end().into()],
            "Extended description line 1"
        );
        assert_eq!(
            &input[value_lines[2].start().into()..value_lines[2].end().into()],
            "Extended description line 2"
        );
    }

    #[test]
    fn test_entries_public_access() {
        let input = r#"Package: test
Version: 1.0"#;

        let deb822 = super::Deb822::from_str(input).unwrap();
        let paragraph = deb822.paragraphs().next().unwrap();

        // Test that entries() method is now public
        let entries: Vec<_> = paragraph.entries().collect();
        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0].key(), Some("Package".to_string()));
        assert_eq!(entries[1].key(), Some("Version".to_string()));
    }

    #[test]
    fn test_empty_value_ranges() {
        let input = r#"EmptyField: "#;

        let deb822 = super::Deb822::from_str(input).unwrap();
        let paragraph = deb822.paragraphs().next().unwrap();
        let entry = paragraph.entries().next().unwrap();

        assert_eq!(entry.key(), Some("EmptyField".to_string()));

        // Empty value should still have ranges
        assert!(entry.key_range().is_some());
        assert!(entry.colon_range().is_some());

        // Empty value might not have value tokens
        let value_lines = entry.value_line_ranges();
        // This depends on how the parser handles empty values
        // but we should not panic
        assert!(value_lines.len() <= 1);
    }

    #[test]
    fn test_range_ordering() {
        let input = r#"Field: value"#;

        let deb822 = super::Deb822::from_str(input).unwrap();
        let paragraph = deb822.paragraphs().next().unwrap();
        let entry = paragraph.entries().next().unwrap();

        let key_range = entry.key_range().unwrap();
        let colon_range = entry.colon_range().unwrap();
        let value_range = entry.value_range().unwrap();
        let text_range = entry.text_range();

        // Verify ranges are in correct order
        assert!(key_range.end() <= colon_range.start());
        assert!(colon_range.end() <= value_range.start());
        assert!(key_range.start() >= text_range.start());
        assert!(value_range.end() <= text_range.end());
    }

    #[test]
    fn test_error_recovery_missing_colon() {
        let input = r#"Source foo
Maintainer: Test User <test@example.com>
"#;
        let (deb822, errors) = super::Deb822::from_str_relaxed(input);

        // Should still parse successfully with errors
        assert!(!errors.is_empty());
        assert!(errors.iter().any(|e| e.contains("missing colon")));

        // Should still have a paragraph with the valid field
        let paragraph = deb822.paragraphs().next().unwrap();
        assert_eq!(
            paragraph.get("Maintainer").as_deref(),
            Some("Test User <test@example.com>")
        );
    }

    #[test]
    fn test_error_recovery_missing_field_name() {
        let input = r#": orphaned value
Package: test
"#;

        let (deb822, errors) = super::Deb822::from_str_relaxed(input);

        // Should have errors about missing field name
        assert!(!errors.is_empty());
        assert!(errors
            .iter()
            .any(|e| e.contains("field name") || e.contains("missing")));

        // The valid field should be in one of the paragraphs
        let paragraphs: Vec<_> = deb822.paragraphs().collect();
        let mut found_package = false;
        for paragraph in paragraphs.iter() {
            if paragraph.get("Package").is_some() {
                found_package = true;
                assert_eq!(paragraph.get("Package").as_deref(), Some("test"));
            }
        }
        assert!(found_package, "Package field not found in any paragraph");
    }

    #[test]
    fn test_error_recovery_orphaned_text() {
        let input = r#"Package: test
some orphaned text without field name
Version: 1.0
"#;
        let (deb822, errors) = super::Deb822::from_str_relaxed(input);

        // Should have errors about orphaned text
        assert!(!errors.is_empty());
        assert!(errors.iter().any(|e| e.contains("orphaned")
            || e.contains("unexpected")
            || e.contains("field name")));

        // Should still parse the valid fields (may be split across paragraphs)
        let mut all_fields = std::collections::HashMap::new();
        for paragraph in deb822.paragraphs() {
            for (key, value) in paragraph.items() {
                all_fields.insert(key, value);
            }
        }

        assert_eq!(all_fields.get("Package"), Some(&"test".to_string()));
        assert_eq!(all_fields.get("Version"), Some(&"1.0".to_string()));
    }

    #[test]
    fn test_error_recovery_consecutive_field_names() {
        let input = r#"Package: test
Description
Maintainer: Another field without proper value
Version: 1.0
"#;
        let (deb822, errors) = super::Deb822::from_str_relaxed(input);

        // Should have errors about missing values
        assert!(!errors.is_empty());
        assert!(errors.iter().any(|e| e.contains("consecutive")
            || e.contains("missing")
            || e.contains("incomplete")));

        // Should still parse valid fields (may be split across paragraphs due to errors)
        let mut all_fields = std::collections::HashMap::new();
        for paragraph in deb822.paragraphs() {
            for (key, value) in paragraph.items() {
                all_fields.insert(key, value);
            }
        }

        assert_eq!(all_fields.get("Package"), Some(&"test".to_string()));
        assert_eq!(
            all_fields.get("Maintainer"),
            Some(&"Another field without proper value".to_string())
        );
        assert_eq!(all_fields.get("Version"), Some(&"1.0".to_string()));
    }

    #[test]
    fn test_error_recovery_malformed_multiline() {
        let input = r#"Package: test
Description: Short desc
  Proper continuation
invalid continuation without indent
 Another proper continuation
Version: 1.0
"#;
        let (deb822, errors) = super::Deb822::from_str_relaxed(input);

        // Should recover from malformed continuation
        assert!(!errors.is_empty());

        // Should still parse other fields correctly
        let paragraph = deb822.paragraphs().next().unwrap();
        assert_eq!(paragraph.get("Package").as_deref(), Some("test"));
        assert_eq!(paragraph.get("Version").as_deref(), Some("1.0"));
    }

    #[test]
    fn test_error_recovery_mixed_errors() {
        let input = r#"Package test without colon
: orphaned colon
Description: Valid field
some orphaned text
Another-Field: Valid too
"#;
        let (deb822, errors) = super::Deb822::from_str_relaxed(input);

        // Should have multiple different errors
        assert!(!errors.is_empty());
        assert!(errors.len() >= 2);

        // Should still parse the valid fields
        let paragraph = deb822.paragraphs().next().unwrap();
        assert_eq!(paragraph.get("Description").as_deref(), Some("Valid field"));
        assert_eq!(paragraph.get("Another-Field").as_deref(), Some("Valid too"));
    }

    #[test]
    fn test_error_recovery_paragraph_boundary() {
        let input = r#"Package: first-package
Description: First paragraph

corrupted data here
: more corruption
completely broken line

Package: second-package
Version: 1.0
"#;
        let (deb822, errors) = super::Deb822::from_str_relaxed(input);

        // Should have errors from the corrupted section
        assert!(!errors.is_empty());

        // Should still parse both paragraphs correctly
        let paragraphs: Vec<_> = deb822.paragraphs().collect();
        assert_eq!(paragraphs.len(), 2);

        assert_eq!(
            paragraphs[0].get("Package").as_deref(),
            Some("first-package")
        );
        assert_eq!(
            paragraphs[1].get("Package").as_deref(),
            Some("second-package")
        );
        assert_eq!(paragraphs[1].get("Version").as_deref(), Some("1.0"));
    }

    #[test]
    fn test_error_recovery_with_positioned_errors() {
        let input = r#"Package test
Description: Valid
"#;
        let parsed = super::parse(input);

        // Should have positioned errors with proper ranges
        assert!(!parsed.positioned_errors.is_empty());

        let first_error = &parsed.positioned_errors[0];
        assert!(!first_error.message.is_empty());
        assert!(first_error.range.start() <= first_error.range.end());
        assert!(first_error.code.is_some());

        // Error should point to the problematic location
        let error_text = &input[first_error.range.start().into()..first_error.range.end().into()];
        assert!(!error_text.is_empty());
    }

    #[test]
    fn test_error_recovery_preserves_whitespace() {
        let input = r#"Source: package
Maintainer   Test User <test@example.com>
Section:    utils

"#;
        let (deb822, errors) = super::Deb822::from_str_relaxed(input);

        // Should have error about missing colon
        assert!(!errors.is_empty());

        // Should preserve original formatting in output
        let output = deb822.to_string();
        assert!(output.contains("Section:    utils"));

        // Should still extract valid fields
        let paragraph = deb822.paragraphs().next().unwrap();
        assert_eq!(paragraph.get("Source").as_deref(), Some("package"));
        assert_eq!(paragraph.get("Section").as_deref(), Some("utils"));
    }

    #[test]
    fn test_error_recovery_empty_fields() {
        let input = r#"Package: test
Description:
Maintainer: Valid User
EmptyField:
Version: 1.0
"#;
        let (deb822, _errors) = super::Deb822::from_str_relaxed(input);

        // Empty fields should parse without major errors - collect all fields from all paragraphs
        let mut all_fields = std::collections::HashMap::new();
        for paragraph in deb822.paragraphs() {
            for (key, value) in paragraph.items() {
                all_fields.insert(key, value);
            }
        }

        assert_eq!(all_fields.get("Package"), Some(&"test".to_string()));
        assert_eq!(all_fields.get("Description"), Some(&"".to_string()));
        assert_eq!(
            all_fields.get("Maintainer"),
            Some(&"Valid User".to_string())
        );
        assert_eq!(all_fields.get("EmptyField"), Some(&"".to_string()));
        assert_eq!(all_fields.get("Version"), Some(&"1.0".to_string()));
    }

    #[test]
    fn test_insert_comment_before() {
        let d: super::Deb822 = vec![
            vec![("Source", "foo"), ("Maintainer", "Bar <bar@example.com>")]
                .into_iter()
                .collect(),
            vec![("Package", "foo"), ("Architecture", "all")]
                .into_iter()
                .collect(),
        ]
        .into_iter()
        .collect();

        // Insert comment before first paragraph
        let mut p1 = d.paragraphs().next().unwrap();
        p1.insert_comment_before("This is the source paragraph");

        // Insert comment before second paragraph
        let mut p2 = d.paragraphs().nth(1).unwrap();
        p2.insert_comment_before("This is the binary paragraph");

        let output = d.to_string();
        assert_eq!(
            output,
            r#"# This is the source paragraph
Source: foo
Maintainer: Bar <bar@example.com>

# This is the binary paragraph
Package: foo
Architecture: all
"#
        );
    }

    #[test]
    fn test_parse_continuation_with_colon() {
        // Test that continuation lines with colons are properly parsed
        let input = "Package: test\nDescription: short\n line: with colon\n";
        let result = input.parse::<Deb822>();
        assert!(result.is_ok());

        let deb822 = result.unwrap();
        let para = deb822.paragraphs().next().unwrap();
        assert_eq!(para.get("Package").as_deref(), Some("test"));
        assert_eq!(
            para.get("Description").as_deref(),
            Some("short\nline: with colon")
        );
    }

    #[test]
    fn test_parse_continuation_starting_with_colon() {
        // Test continuation line STARTING with a colon (issue #315)
        let input = "Package: test\nDescription: short\n :value\n";
        let result = input.parse::<Deb822>();
        assert!(result.is_ok());

        let deb822 = result.unwrap();
        let para = deb822.paragraphs().next().unwrap();
        assert_eq!(para.get("Package").as_deref(), Some("test"));
        assert_eq!(para.get("Description").as_deref(), Some("short\n:value"));
    }

    #[test]
    fn test_normalize_field_spacing_single_space() {
        // Field already has correct spacing
        let input = "Field: value\n";
        let deb822 = input.parse::<Deb822>().unwrap();
        let mut para = deb822.paragraphs().next().unwrap();

        para.normalize_field_spacing();
        assert_eq!(para.to_string(), "Field: value\n");
    }

    #[test]
    fn test_normalize_field_spacing_extra_spaces() {
        // Field has extra spaces after colon
        let input = "Field:    value\n";
        let deb822 = input.parse::<Deb822>().unwrap();
        let mut para = deb822.paragraphs().next().unwrap();

        para.normalize_field_spacing();
        assert_eq!(para.to_string(), "Field: value\n");
    }

    #[test]
    fn test_normalize_field_spacing_no_space() {
        // Field has no space after colon
        let input = "Field:value\n";
        let deb822 = input.parse::<Deb822>().unwrap();
        let mut para = deb822.paragraphs().next().unwrap();

        para.normalize_field_spacing();
        assert_eq!(para.to_string(), "Field: value\n");
    }

    #[test]
    fn test_normalize_field_spacing_multiple_fields() {
        // Multiple fields with various spacing
        let input = "Field1:    value1\nField2:value2\nField3:  value3\n";
        let deb822 = input.parse::<Deb822>().unwrap();
        let mut para = deb822.paragraphs().next().unwrap();

        para.normalize_field_spacing();
        assert_eq!(
            para.to_string(),
            "Field1: value1\nField2: value2\nField3: value3\n"
        );
    }

    #[test]
    fn test_normalize_field_spacing_multiline_value() {
        // Field with multiline value
        let input = "Description:    short\n continuation line\n .  \n final line\n";
        let deb822 = input.parse::<Deb822>().unwrap();
        let mut para = deb822.paragraphs().next().unwrap();

        para.normalize_field_spacing();
        assert_eq!(
            para.to_string(),
            "Description: short\n continuation line\n .  \n final line\n"
        );
    }

    #[test]
    fn test_normalize_field_spacing_empty_value_with_whitespace() {
        // Field with empty value (only whitespace) should normalize to no space
        let input = "Field:  \n";
        let deb822 = input.parse::<Deb822>().unwrap();
        let mut para = deb822.paragraphs().next().unwrap();

        para.normalize_field_spacing();
        // When value is empty/whitespace-only, normalize to no space
        assert_eq!(para.to_string(), "Field:\n");
    }

    #[test]
    fn test_normalize_field_spacing_no_value() {
        // Field with no value (just newline) should stay unchanged
        let input = "Depends:\n";
        let deb822 = input.parse::<Deb822>().unwrap();
        let mut para = deb822.paragraphs().next().unwrap();

        para.normalize_field_spacing();
        // Should remain with no space
        assert_eq!(para.to_string(), "Depends:\n");
    }

    #[test]
    fn test_normalize_field_spacing_multiple_paragraphs() {
        // Multiple paragraphs
        let input = "Field1:    value1\n\nField2:  value2\n";
        let mut deb822 = input.parse::<Deb822>().unwrap();

        deb822.normalize_field_spacing();
        assert_eq!(deb822.to_string(), "Field1: value1\n\nField2: value2\n");
    }

    #[test]
    fn test_normalize_field_spacing_preserves_comments() {
        // Normalize spacing while preserving comments (comments are at document level)
        let input = "# Comment\nField:    value\n";
        let mut deb822 = input.parse::<Deb822>().unwrap();

        deb822.normalize_field_spacing();
        assert_eq!(deb822.to_string(), "# Comment\nField: value\n");
    }

    #[test]
    fn test_normalize_field_spacing_preserves_values() {
        // Ensure values are preserved exactly
        let input = "Source:   foo-bar\nMaintainer:Foo Bar <test@example.com>\n";
        let deb822 = input.parse::<Deb822>().unwrap();
        let mut para = deb822.paragraphs().next().unwrap();

        para.normalize_field_spacing();

        assert_eq!(para.get("Source").as_deref(), Some("foo-bar"));
        assert_eq!(
            para.get("Maintainer").as_deref(),
            Some("Foo Bar <test@example.com>")
        );
    }

    #[test]
    fn test_normalize_field_spacing_tab_after_colon() {
        // Field with tab after colon (should be normalized to single space)
        let input = "Field:\tvalue\n";
        let deb822 = input.parse::<Deb822>().unwrap();
        let mut para = deb822.paragraphs().next().unwrap();

        para.normalize_field_spacing();
        assert_eq!(para.to_string(), "Field: value\n");
    }

    #[test]
    fn test_set_preserves_indentation() {
        // Test that Paragraph.set() preserves the original indentation
        let original = r#"Source: example
Build-Depends: foo,
               bar,
               baz
"#;

        let mut para: super::Paragraph = original.parse().unwrap();

        // Modify the Build-Depends field
        para.set("Build-Depends", "foo,\nbar,\nbaz");

        // The indentation should be preserved (15 spaces for "Build-Depends: ")
        let expected = r#"Source: example
Build-Depends: foo,
               bar,
               baz
"#;
        assert_eq!(para.to_string(), expected);
    }

    #[test]
    fn test_set_new_field_detects_field_name_length_indent() {
        // Test that new fields detect field-name-length-based indentation
        let original = r#"Source: example
Build-Depends: foo,
               bar,
               baz
Depends: lib1,
         lib2
"#;

        let mut para: super::Paragraph = original.parse().unwrap();

        // Add a new multi-line field - should detect that indentation is field-name-length + 2
        para.set("Recommends", "pkg1,\npkg2,\npkg3");

        // "Recommends: " is 12 characters, so indentation should be 12 spaces
        assert!(para
            .to_string()
            .contains("Recommends: pkg1,\n            pkg2,"));
    }

    #[test]
    fn test_set_new_field_detects_fixed_indent() {
        // Test that new fields detect fixed indentation pattern
        let original = r#"Source: example
Build-Depends: foo,
     bar,
     baz
Depends: lib1,
     lib2
"#;

        let mut para: super::Paragraph = original.parse().unwrap();

        // Add a new multi-line field - should detect fixed 5-space indentation
        para.set("Recommends", "pkg1,\npkg2,\npkg3");

        // Should use the same 5-space indentation
        assert!(para
            .to_string()
            .contains("Recommends: pkg1,\n     pkg2,\n     pkg3\n"));
    }

    #[test]
    fn test_set_new_field_no_multiline_fields() {
        // Test that new fields use field-name-length when no existing multi-line fields
        let original = r#"Source: example
Maintainer: Test <test@example.com>
"#;

        let mut para: super::Paragraph = original.parse().unwrap();

        // Add a new multi-line field - should default to field name length + 2
        para.set("Depends", "foo,\nbar,\nbaz");

        // "Depends: " is 9 characters, so indentation should be 9 spaces
        let expected = r#"Source: example
Maintainer: Test <test@example.com>
Depends: foo,
         bar,
         baz
"#;
        assert_eq!(para.to_string(), expected);
    }

    #[test]
    fn test_set_new_field_mixed_indentation() {
        // Test that new fields fall back to field-name-length when pattern is inconsistent
        let original = r#"Source: example
Build-Depends: foo,
               bar
Depends: lib1,
     lib2
"#;

        let mut para: super::Paragraph = original.parse().unwrap();

        // Add a new multi-line field - mixed pattern, should fall back to field name length + 2
        para.set("Recommends", "pkg1,\npkg2");

        // "Recommends: " is 12 characters
        assert!(para
            .to_string()
            .contains("Recommends: pkg1,\n            pkg2\n"));
    }

    #[test]
    fn test_entry_with_indentation() {
        // Test Entry::with_indentation directly
        let entry = super::Entry::with_indentation("Test-Field", "value1\nvalue2\nvalue3", "    ");

        assert_eq!(
            entry.to_string(),
            "Test-Field: value1\n    value2\n    value3\n"
        );
    }

    #[test]
    fn test_set_with_indent_pattern_fixed() {
        // Test setting a field with explicit fixed indentation pattern
        let original = r#"Source: example
Maintainer: Test <test@example.com>
"#;

        let mut para: super::Paragraph = original.parse().unwrap();

        // Add a new multi-line field with fixed 4-space indentation
        para.set_with_indent_pattern(
            "Depends",
            "foo,\nbar,\nbaz",
            Some(&super::IndentPattern::Fixed(4)),
            None,
        );

        // Should use the specified 4-space indentation
        let expected = r#"Source: example
Maintainer: Test <test@example.com>
Depends: foo,
    bar,
    baz
"#;
        assert_eq!(para.to_string(), expected);
    }

    #[test]
    fn test_set_with_indent_pattern_field_name_length() {
        // Test setting a field with field-name-length indentation pattern
        let original = r#"Source: example
Maintainer: Test <test@example.com>
"#;

        let mut para: super::Paragraph = original.parse().unwrap();

        // Add a new multi-line field with field-name-length indentation
        para.set_with_indent_pattern(
            "Build-Depends",
            "libfoo,\nlibbar,\nlibbaz",
            Some(&super::IndentPattern::FieldNameLength),
            None,
        );

        // "Build-Depends: " is 15 characters, so indentation should be 15 spaces
        let expected = r#"Source: example
Maintainer: Test <test@example.com>
Build-Depends: libfoo,
               libbar,
               libbaz
"#;
        assert_eq!(para.to_string(), expected);
    }

    #[test]
    fn test_set_with_indent_pattern_override_auto_detection() {
        // Test that explicit default pattern overrides auto-detection for new fields
        let original = r#"Source: example
Build-Depends: foo,
               bar,
               baz
"#;

        let mut para: super::Paragraph = original.parse().unwrap();

        // Add a NEW field with fixed 2-space indentation, overriding the auto-detected pattern
        para.set_with_indent_pattern(
            "Depends",
            "lib1,\nlib2,\nlib3",
            Some(&super::IndentPattern::Fixed(2)),
            None,
        );

        // Should use the specified 2-space indentation, not the auto-detected 15-space
        let expected = r#"Source: example
Build-Depends: foo,
               bar,
               baz
Depends: lib1,
  lib2,
  lib3
"#;
        assert_eq!(para.to_string(), expected);
    }

    #[test]
    fn test_set_with_indent_pattern_none_auto_detects() {
        // Test that None pattern auto-detects from existing fields
        let original = r#"Source: example
Build-Depends: foo,
     bar,
     baz
"#;

        let mut para: super::Paragraph = original.parse().unwrap();

        // Add a field with None pattern - should auto-detect fixed 5-space
        para.set_with_indent_pattern("Depends", "lib1,\nlib2", None, None);

        // Should auto-detect and use the 5-space indentation
        let expected = r#"Source: example
Build-Depends: foo,
     bar,
     baz
Depends: lib1,
     lib2
"#;
        assert_eq!(para.to_string(), expected);
    }

    #[test]
    fn test_set_with_indent_pattern_with_field_order() {
        // Test setting a field with both indent pattern and field ordering
        let original = r#"Source: example
Maintainer: Test <test@example.com>
"#;

        let mut para: super::Paragraph = original.parse().unwrap();

        // Add a field with fixed indentation and specific field ordering
        para.set_with_indent_pattern(
            "Priority",
            "optional",
            Some(&super::IndentPattern::Fixed(4)),
            Some(&["Source", "Priority", "Maintainer"]),
        );

        // Priority should be inserted between Source and Maintainer
        let expected = r#"Source: example
Priority: optional
Maintainer: Test <test@example.com>
"#;
        assert_eq!(para.to_string(), expected);
    }

    #[test]
    fn test_set_with_indent_pattern_replace_existing() {
        // Test that replacing an existing multi-line field preserves its indentation
        let original = r#"Source: example
Depends: foo,
         bar
"#;

        let mut para: super::Paragraph = original.parse().unwrap();

        // Replace Depends - the default pattern is ignored, existing indentation is preserved
        para.set_with_indent_pattern(
            "Depends",
            "lib1,\nlib2,\nlib3",
            Some(&super::IndentPattern::Fixed(3)),
            None,
        );

        // Should preserve the existing 9-space indentation, not use the default 3-space
        let expected = r#"Source: example
Depends: lib1,
         lib2,
         lib3
"#;
        assert_eq!(para.to_string(), expected);
    }

    #[test]
    fn test_entry_get_indent() {
        // Test that we can extract indentation from an entry
        let original = r#"Build-Depends: foo,
               bar,
               baz
"#;
        let para: super::Paragraph = original.parse().unwrap();
        let entry = para.entries().next().unwrap();

        assert_eq!(entry.get_indent(), Some("               ".to_string()));
    }

    #[test]
    fn test_entry_get_indent_single_line() {
        // Single-line entries should return None for indentation
        let original = r#"Source: example
"#;
        let para: super::Paragraph = original.parse().unwrap();
        let entry = para.entries().next().unwrap();

        assert_eq!(entry.get_indent(), None);
    }
}

#[test]
fn test_move_paragraph_forward() {
    let mut d: Deb822 = vec![
        vec![("Foo", "Bar"), ("Baz", "Qux")].into_iter().collect(),
        vec![("A", "B"), ("C", "D")].into_iter().collect(),
        vec![("X", "Y"), ("Z", "W")].into_iter().collect(),
    ]
    .into_iter()
    .collect();
    d.move_paragraph(0, 2);
    assert_eq!(
        d.to_string(),
        "A: B\nC: D\n\nX: Y\nZ: W\n\nFoo: Bar\nBaz: Qux\n"
    );
}

#[test]
fn test_move_paragraph_backward() {
    let mut d: Deb822 = vec![
        vec![("Foo", "Bar"), ("Baz", "Qux")].into_iter().collect(),
        vec![("A", "B"), ("C", "D")].into_iter().collect(),
        vec![("X", "Y"), ("Z", "W")].into_iter().collect(),
    ]
    .into_iter()
    .collect();
    d.move_paragraph(2, 0);
    assert_eq!(
        d.to_string(),
        "X: Y\nZ: W\n\nFoo: Bar\nBaz: Qux\n\nA: B\nC: D\n"
    );
}

#[test]
fn test_move_paragraph_middle() {
    let mut d: Deb822 = vec![
        vec![("Foo", "Bar"), ("Baz", "Qux")].into_iter().collect(),
        vec![("A", "B"), ("C", "D")].into_iter().collect(),
        vec![("X", "Y"), ("Z", "W")].into_iter().collect(),
    ]
    .into_iter()
    .collect();
    d.move_paragraph(2, 1);
    assert_eq!(
        d.to_string(),
        "Foo: Bar\nBaz: Qux\n\nX: Y\nZ: W\n\nA: B\nC: D\n"
    );
}

#[test]
fn test_move_paragraph_same_index() {
    let mut d: Deb822 = vec![
        vec![("Foo", "Bar"), ("Baz", "Qux")].into_iter().collect(),
        vec![("A", "B"), ("C", "D")].into_iter().collect(),
    ]
    .into_iter()
    .collect();
    let original = d.to_string();
    d.move_paragraph(1, 1);
    assert_eq!(d.to_string(), original);
}

#[test]
fn test_move_paragraph_single() {
    let mut d: Deb822 = vec![vec![("Foo", "Bar")].into_iter().collect()]
        .into_iter()
        .collect();
    let original = d.to_string();
    d.move_paragraph(0, 0);
    assert_eq!(d.to_string(), original);
}

#[test]
fn test_move_paragraph_invalid_index() {
    let mut d: Deb822 = vec![
        vec![("Foo", "Bar")].into_iter().collect(),
        vec![("A", "B")].into_iter().collect(),
    ]
    .into_iter()
    .collect();
    let original = d.to_string();
    d.move_paragraph(0, 5);
    assert_eq!(d.to_string(), original);
}

#[test]
fn test_move_paragraph_with_comments() {
    let text = r#"Foo: Bar

# This is a comment

A: B

X: Y
"#;
    let mut d: Deb822 = text.parse().unwrap();
    d.move_paragraph(0, 2);
    assert_eq!(
        d.to_string(),
        "# This is a comment\n\nA: B\n\nX: Y\n\nFoo: Bar\n"
    );
}
