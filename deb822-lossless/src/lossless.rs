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

/// List of encountered syntax errors.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseError(Vec<String>);

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
struct Parse {
    green_node: GreenNode,
    #[allow(unused)]
    errors: Vec<String>,
}

fn parse(text: &str) -> Parse {
    struct Parser<'a> {
        /// input tokens, including whitespace,
        /// in *reverse* order.
        tokens: Vec<(SyntaxKind, &'a str)>,
        /// the in-progress tree.
        builder: GreenNodeBuilder<'static>,
        /// the list of syntax errors we've accumulated
        /// so far.
        errors: Vec<String>,
    }

    impl<'a> Parser<'a> {
        fn parse_entry(&mut self) {
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
                        self.bump();
                        self.errors.push(format!("expected newline, got {g:?}"));
                        self.builder.finish_node();
                    }
                }
            }

            self.builder.start_node(ENTRY.into());

            // First, parse the key and colon
            if self.current() == Some(KEY) {
                self.bump();
                self.skip_ws();
            } else {
                self.builder.start_node(ERROR.into());
                if self.current().is_some() {
                    self.bump();
                }
                self.errors.push("expected key".to_string());
                self.builder.finish_node();
            }
            if self.current() == Some(COLON) {
                self.bump();
                self.skip_ws();
            } else {
                self.builder.start_node(ERROR.into());
                if self.current().is_some() {
                    self.bump();
                }
                self.errors
                    .push(format!("expected ':', got {:?}", self.current()));
                self.builder.finish_node();
            }
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
                    Some(g) => {
                        self.builder.start_node(ERROR.into());
                        self.bump();
                        self.errors.push(format!("expected newline, got {g:?}"));
                        self.builder.finish_node();
                    }
                }
                if self.current() == Some(INDENT) {
                    self.bump();
                    self.skip_ws();
                } else {
                    break;
                }
            }
            self.builder.finish_node();
        }

        fn parse_paragraph(&mut self) {
            self.builder.start_node(PARAGRAPH.into());
            while self.current() != Some(NEWLINE) && self.current().is_some() {
                self.parse_entry();
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
            }
        }
        /// Advance one token, adding it to the current branch of the tree builder.
        fn bump(&mut self) {
            let (kind, text) = self.tokens.pop().unwrap();
            self.builder.token(kind.into(), text);
        }
        /// Peek at the first unprocessed token
        fn current(&self) -> Option<SyntaxKind> {
            self.tokens.last().map(|(kind, _)| *kind)
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
    tokens.reverse();
    Parser {
        tokens,
        builder: GreenNodeBuilder::new(),
        errors: Vec::new(),
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
        #[derive(Debug, PartialEq, Eq, Hash)]
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

    /// Add a new empty paragraph to the end of the file.
    pub fn add_paragraph(&mut self) -> Paragraph {
        self.insert_empty_paragraph(None)
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
    fn entries(&self) -> impl Iterator<Item = Entry> + '_ {
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

    /// Set a field in the paragraph
    pub fn set(&mut self, key: &str, value: &str) {
        let new_entry = Entry::new(key, value);

        for entry in self.entries() {
            if entry.key().as_deref() == Some(key) {
                self.0.splice_children(
                    entry.0.index()..entry.0.index() + 1,
                    vec![new_entry.0.into()],
                );
                return;
            }
        }
        let count = self.0.children_with_tokens().count();
        self.0
            .splice_children(count..count, vec![new_entry.0.into()]);
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
impl pyo3::FromPyObject<'_> for Paragraph {
    fn extract_bound(obj: &pyo3::Bound<pyo3::PyAny>) -> pyo3::PyResult<Self> {
        use pyo3::prelude::*;
        let d = obj.call_method0("__str__")?.extract::<String>()?;
        Ok(Paragraph::from_str(&d)
            .map_err(|e| pyo3::exceptions::PyValueError::new_err((e.to_string(),)))?)
    }
}

impl Entry {
    /// Create a new entry with the given key and value.
    pub fn new(key: &str, value: &str) -> Entry {
        let mut builder = GreenNodeBuilder::new();

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

    /// Detach this entry from the paragraph.
    pub fn detach(&mut self) {
        self.0.detach();
    }
}

impl FromStr for Deb822 {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parsed = parse(s);
        if parsed.errors.is_empty() {
            Ok(parsed.root_mut())
        } else {
            Err(ParseError(parsed.errors))
        }
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
}
