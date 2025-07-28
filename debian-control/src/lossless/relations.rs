//! Parser for relationship fields like `Depends`, `Recommends`, etc.
//!
//! # Example
//! ```
//! use debian_control::lossless::relations::{Relations, Relation};
//! use debian_control::relations::VersionConstraint;
//!
//! let mut relations: Relations = r"python3-dulwich (>= 0.19.0), python3-requests, python3-urllib3 (<< 1.26.0)".parse().unwrap();
//! assert_eq!(relations.to_string(), "python3-dulwich (>= 0.19.0), python3-requests, python3-urllib3 (<< 1.26.0)");
//! assert!(relations.satisfied_by(|name: &str| -> Option<debversion::Version> {
//!    match name {
//!    "python3-dulwich" => Some("0.19.0".parse().unwrap()),
//!    "python3-requests" => Some("2.25.1".parse().unwrap()),
//!    "python3-urllib3" => Some("1.25.11".parse().unwrap()),
//!    _ => None
//!    }}));
//! relations.remove_entry(1);
//! relations.get_entry(0).unwrap().get_relation(0).unwrap().set_archqual("amd64");
//! assert_eq!(relations.to_string(), "python3-dulwich:amd64 (>= 0.19.0), python3-urllib3 (<< 1.26.0)");
//! ```
use crate::relations::SyntaxKind::{self, *};
use crate::relations::{BuildProfile, VersionConstraint};
use debversion::Version;
use rowan::{Direction, NodeOrToken};
use std::collections::HashSet;

/// Error type for parsing relations fields
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

/// Second, implementing the `Language` trait teaches rowan to convert between
/// these two SyntaxKind types, allowing for a nicer SyntaxNode API where
/// "kinds" are values from our `enum SyntaxKind`, instead of plain u16 values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Lang {}
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
use rowan::{GreenNode, GreenToken};

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

fn parse(text: &str, allow_substvar: bool) -> Parse {
    struct Parser {
        /// input tokens, including whitespace,
        /// in *reverse* order.
        tokens: Vec<(SyntaxKind, String)>,
        /// the in-progress tree.
        builder: GreenNodeBuilder<'static>,
        /// the list of syntax errors we've accumulated
        /// so far.
        errors: Vec<String>,
        /// whether to allow substvars
        allow_substvar: bool,
    }

    impl Parser {
        fn parse_substvar(&mut self) {
            self.builder.start_node(SyntaxKind::SUBSTVAR.into());
            self.bump();
            if self.current() != Some(L_CURLY) {
                self.error(format!("expected {{ but got {:?}", self.current()).to_string());
            } else {
                self.bump();
            }
            loop {
                match self.current() {
                    Some(IDENT) | Some(COLON) => {
                        self.bump();
                    }
                    Some(R_CURLY) => {
                        break;
                    }
                    e => {
                        self.error(format!("expected identifier or : but got {:?}", e).to_string());
                    }
                }
            }
            if self.current() != Some(R_CURLY) {
                self.error(format!("expected }} but got {:?}", self.current()).to_string());
            } else {
                self.bump();
            }
            self.builder.finish_node();
        }

        fn parse_entry(&mut self) {
            self.skip_ws();
            self.builder.start_node(SyntaxKind::ENTRY.into());
            loop {
                self.parse_relation();
                match self.peek_past_ws() {
                    Some(COMMA) => {
                        break;
                    }
                    Some(PIPE) => {
                        self.skip_ws();
                        self.bump();
                        self.skip_ws();
                    }
                    None => {
                        self.skip_ws();
                        break;
                    }
                    _ => {
                        self.skip_ws();
                        self.builder.start_node(SyntaxKind::ERROR.into());
                        match self.tokens.pop() {
                            Some((k, t)) => {
                                self.builder.token(k.into(), t.as_str());
                                self.errors
                                    .push(format!("Expected comma or pipe, not {:?}", (k, t)));
                            }
                            None => {
                                self.errors
                                    .push("Expected comma or pipe, got end of file".to_string());
                            }
                        }
                        self.builder.finish_node();
                    }
                }
            }
            self.builder.finish_node();
        }

        fn error(&mut self, error: String) {
            self.errors.push(error);
            self.builder.start_node(SyntaxKind::ERROR.into());
            if self.current().is_some() {
                self.bump();
            }
            self.builder.finish_node();
        }

        fn parse_relation(&mut self) {
            self.builder.start_node(SyntaxKind::RELATION.into());
            if self.current() == Some(IDENT) {
                self.bump();
            } else {
                self.error("Expected package name".to_string());
            }
            match self.peek_past_ws() {
                Some(COLON) => {
                    self.skip_ws();
                    self.builder.start_node(ARCHQUAL.into());
                    self.bump();
                    self.skip_ws();
                    if self.current() == Some(IDENT) {
                        self.bump();
                    } else {
                        self.error("Expected architecture name".to_string());
                    }
                    self.builder.finish_node();
                    self.skip_ws();
                }
                Some(PIPE) | Some(COMMA) => {}
                None | Some(L_PARENS) | Some(L_BRACKET) | Some(L_ANGLE) => {
                    self.skip_ws();
                }
                e => {
                    self.skip_ws();
                    self.error(format!(
                        "Expected ':' or '|' or '[' or '<' or ',' but got {:?}",
                        e
                    ));
                }
            }

            if self.peek_past_ws() == Some(L_PARENS) {
                self.skip_ws();
                self.builder.start_node(VERSION.into());
                self.bump();
                self.skip_ws();

                self.builder.start_node(CONSTRAINT.into());

                while self.current() == Some(L_ANGLE)
                    || self.current() == Some(R_ANGLE)
                    || self.current() == Some(EQUAL)
                {
                    self.bump();
                }

                self.builder.finish_node();

                self.skip_ws();

                if self.current() == Some(IDENT) {
                    self.bump();
                } else {
                    self.error("Expected version".to_string());
                }

                if self.current() == Some(R_PARENS) {
                    self.bump();
                } else {
                    self.error("Expected ')'".to_string());
                }

                self.builder.finish_node();
            }

            if self.peek_past_ws() == Some(L_BRACKET) {
                self.skip_ws();
                self.builder.start_node(ARCHITECTURES.into());
                self.bump();
                loop {
                    self.skip_ws();
                    match self.current() {
                        Some(NOT) => {
                            self.bump();
                        }
                        Some(IDENT) => {
                            self.bump();
                        }
                        Some(R_BRACKET) => {
                            self.bump();
                            break;
                        }
                        _ => {
                            self.error("Expected architecture name or '!' or ']'".to_string());
                        }
                    }
                }
                self.builder.finish_node();
            }

            while self.peek_past_ws() == Some(L_ANGLE) {
                self.skip_ws();
                self.builder.start_node(PROFILES.into());
                self.bump();

                loop {
                    self.skip_ws();
                    match self.current() {
                        Some(IDENT) => {
                            self.bump();
                        }
                        Some(NOT) => {
                            self.bump();
                            self.skip_ws();
                            if self.current() == Some(IDENT) {
                                self.bump();
                            } else {
                                self.error("Expected profile".to_string());
                            }
                        }
                        Some(R_ANGLE) => {
                            self.bump();
                            break;
                        }
                        None => {
                            self.error("Expected profile or '>'".to_string());
                            break;
                        }
                        _ => {
                            self.error("Expected profile or '!' or '>'".to_string());
                        }
                    }
                }

                self.builder.finish_node();
            }

            self.builder.finish_node();
        }

        fn parse(mut self) -> Parse {
            self.builder.start_node(SyntaxKind::ROOT.into());

            self.skip_ws();

            while self.current().is_some() {
                match self.current() {
                    Some(IDENT) => self.parse_entry(),
                    Some(DOLLAR) => {
                        if self.allow_substvar {
                            self.parse_substvar()
                        } else {
                            self.error("Substvars are not allowed".to_string());
                        }
                    }
                    Some(COMMA) => {
                        // Empty entry, but that's okay - probably?
                    }
                    Some(c) => {
                        self.error(format!("expected $ or identifier but got {:?}", c));
                    }
                    None => {
                        self.error("expected identifier but got end of file".to_string());
                    }
                }

                self.skip_ws();
                match self.current() {
                    Some(COMMA) => {
                        self.bump();
                    }
                    None => {
                        break;
                    }
                    c => {
                        self.error(format!("expected comma or end of file but got {:?}", c));
                    }
                }
                self.skip_ws();
            }

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
            self.builder.token(kind.into(), text.as_str());
        }
        /// Peek at the first unprocessed token
        fn current(&self) -> Option<SyntaxKind> {
            self.tokens.last().map(|(kind, _)| *kind)
        }
        fn skip_ws(&mut self) {
            while self.current() == Some(WHITESPACE) || self.current() == Some(NEWLINE) {
                self.bump()
            }
        }

        fn peek_past_ws(&self) -> Option<SyntaxKind> {
            let mut i = self.tokens.len();
            while i > 0 {
                i -= 1;
                match self.tokens[i].0 {
                    WHITESPACE | NEWLINE => {}
                    _ => return Some(self.tokens[i].0),
                }
            }
            None
        }
    }

    let mut tokens = crate::relations::lex(text);
    tokens.reverse();
    Parser {
        tokens,
        builder: GreenNodeBuilder::new(),
        errors: Vec::new(),
        allow_substvar,
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
    fn root_mut(&self) -> Relations {
        Relations::cast(SyntaxNode::new_root_mut(self.green_node.clone())).unwrap()
    }
}

macro_rules! ast_node {
    ($ast:ident, $kind:ident) => {
        /// A node in the syntax tree representing a $ast
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

        impl std::fmt::Display for $ast {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str(&self.0.text().to_string())
            }
        }
    };
}

ast_node!(Relations, ROOT);
ast_node!(Entry, ENTRY);
ast_node!(Relation, RELATION);
ast_node!(Substvar, SUBSTVAR);

impl PartialEq for Relations {
    fn eq(&self, other: &Self) -> bool {
        self.entries().collect::<Vec<_>>() == other.entries().collect::<Vec<_>>()
    }
}

impl PartialEq for Entry {
    fn eq(&self, other: &Self) -> bool {
        self.relations().collect::<Vec<_>>() == other.relations().collect::<Vec<_>>()
    }
}

impl PartialEq for Relation {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name()
            && self.version() == other.version()
            && self.archqual() == other.archqual()
            && self.architectures().map(|x| x.collect::<HashSet<_>>())
                == other.architectures().map(|x| x.collect::<HashSet<_>>())
            && self.profiles().eq(other.profiles())
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for Relations {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let rep = self.to_string();
        serializer.serialize_str(&rep)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for Relations {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        let relations = s.parse().map_err(serde::de::Error::custom)?;
        Ok(relations)
    }
}

impl std::fmt::Debug for Relations {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = f.debug_struct("Relations");

        for entry in self.entries() {
            s.field("entry", &entry);
        }

        s.finish()
    }
}

impl std::fmt::Debug for Entry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = f.debug_struct("Entry");

        for relation in self.relations() {
            s.field("relation", &relation);
        }

        s.finish()
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for Entry {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let rep = self.to_string();
        serializer.serialize_str(&rep)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for Entry {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        let entry = s.parse().map_err(serde::de::Error::custom)?;
        Ok(entry)
    }
}

impl std::fmt::Debug for Relation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = f.debug_struct("Relation");

        s.field("name", &self.name());

        if let Some((vc, version)) = self.version() {
            s.field("version", &vc);
            s.field("version", &version);
        }

        s.finish()
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for Relation {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let rep = self.to_string();
        serializer.serialize_str(&rep)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::Deserialize<'de> for Relation {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        let relation = s.parse().map_err(serde::de::Error::custom)?;
        Ok(relation)
    }
}

impl Default for Relations {
    fn default() -> Self {
        Self::new()
    }
}

impl Relations {
    /// Create a new relations field
    pub fn new() -> Self {
        Self::from(vec![])
    }

    /// Wrap and sort this relations field
    #[must_use]
    pub fn wrap_and_sort(self) -> Self {
        let mut entries = self
            .entries()
            .map(|e| e.wrap_and_sort())
            .collect::<Vec<_>>();
        entries.sort();
        // TODO: preserve comments
        Self::from(entries)
    }

    /// Iterate over the entries in this relations field
    pub fn entries(&self) -> impl Iterator<Item = Entry> + '_ {
        self.0.children().filter_map(Entry::cast)
    }

    /// Iterate over the entries in this relations field
    pub fn iter(&self) -> impl Iterator<Item = Entry> + '_ {
        self.entries()
    }

    /// Remove the entry at the given index
    pub fn get_entry(&self, idx: usize) -> Option<Entry> {
        self.entries().nth(idx)
    }

    /// Remove the entry at the given index
    pub fn remove_entry(&mut self, idx: usize) -> Entry {
        let mut entry = self.get_entry(idx).unwrap();
        entry.remove();
        entry
    }

    /// Insert a new entry at the given index
    pub fn insert(&mut self, idx: usize, entry: Entry) {
        let is_empty = !self.0.children_with_tokens().any(|n| n.kind() == COMMA);
        let (position, new_children) = if let Some(current_entry) = self.entries().nth(idx) {
            let to_insert: Vec<NodeOrToken<GreenNode, GreenToken>> = if idx == 0 && is_empty {
                vec![entry.0.green().into()]
            } else {
                vec![
                    entry.0.green().into(),
                    NodeOrToken::Token(GreenToken::new(COMMA.into(), ",")),
                    NodeOrToken::Token(GreenToken::new(WHITESPACE.into(), " ")),
                ]
            };

            (current_entry.0.index(), to_insert)
        } else {
            let child_count = self.0.children_with_tokens().count();
            (
                child_count,
                if idx == 0 {
                    vec![entry.0.green().into()]
                } else {
                    vec![
                        NodeOrToken::Token(GreenToken::new(COMMA.into(), ",")),
                        NodeOrToken::Token(GreenToken::new(WHITESPACE.into(), " ")),
                        entry.0.green().into(),
                    ]
                },
            )
        };
        // We can safely replace the root here since Relations is a root node
        self.0 = SyntaxNode::new_root_mut(
            self.0.replace_with(
                self.0
                    .green()
                    .splice_children(position..position, new_children),
            ),
        );
    }

    /// Replace the entry at the given index
    pub fn replace(&mut self, idx: usize, entry: Entry) {
        let current_entry = self.get_entry(idx).unwrap();
        self.0.splice_children(
            current_entry.0.index()..current_entry.0.index() + 1,
            vec![entry.0.into()],
        );
    }

    /// Push a new entry to the relations field
    pub fn push(&mut self, entry: Entry) {
        let pos = self.entries().count();
        self.insert(pos, entry);
    }

    /// Return the names of substvars in this relations field
    pub fn substvars(&self) -> impl Iterator<Item = String> + '_ {
        self.0
            .children()
            .filter_map(Substvar::cast)
            .map(|s| s.to_string())
    }

    /// Parse a relations field from a string, allowing syntax errors
    pub fn parse_relaxed(s: &str, allow_substvar: bool) -> (Relations, Vec<String>) {
        let parse = parse(s, allow_substvar);
        (parse.root_mut(), parse.errors)
    }

    /// Check if this relations field is satisfied by the given package versions.
    pub fn satisfied_by(&self, package_version: impl crate::VersionLookup + Copy) -> bool {
        self.entries().all(|e| e.satisfied_by(package_version))
    }

    /// Check if this relations field is empty
    pub fn is_empty(&self) -> bool {
        self.entries().count() == 0
    }

    /// Get the number of entries in this relations field
    pub fn len(&self) -> usize {
        self.entries().count()
    }
}

impl From<Vec<Entry>> for Relations {
    fn from(entries: Vec<Entry>) -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(ROOT.into());
        for (i, entry) in entries.into_iter().enumerate() {
            if i > 0 {
                builder.token(COMMA.into(), ",");
                builder.token(WHITESPACE.into(), " ");
            }
            inject(&mut builder, entry.0);
        }
        builder.finish_node();
        Relations(SyntaxNode::new_root_mut(builder.finish()))
    }
}

impl From<Entry> for Relations {
    fn from(entry: Entry) -> Self {
        Self::from(vec![entry])
    }
}

impl Default for Entry {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialOrd for Entry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let mut rels_a = self.relations();
        let mut rels_b = other.relations();
        while let (Some(a), Some(b)) = (rels_a.next(), rels_b.next()) {
            match a.cmp(&b) {
                std::cmp::Ordering::Equal => continue,
                x => return Some(x),
            }
        }

        if rels_a.next().is_some() {
            return Some(std::cmp::Ordering::Greater);
        }

        if rels_b.next().is_some() {
            return Some(std::cmp::Ordering::Less);
        }

        Some(std::cmp::Ordering::Equal)
    }
}

impl Eq for Entry {}

impl Ord for Entry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Entry {
    /// Create a new entry
    pub fn new() -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::ENTRY.into());
        builder.finish_node();
        Entry(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Replace the relation at the given index
    pub fn replace(&mut self, idx: usize, relation: Relation) {
        let current_relation = self.get_relation(idx).unwrap();

        let old_root = current_relation.0;
        let new_root = relation.0;
        // Preserve white the current relation has
        let mut prev = new_root.first_child_or_token();
        let mut new_head_len = 0;
        // First, strip off any whitespace from the new relation
        while let Some(p) = prev {
            if p.kind() == WHITESPACE || p.kind() == NEWLINE {
                new_head_len += 1;
                prev = p.next_sibling_or_token();
            } else {
                break;
            }
        }
        let mut new_tail_len = 0;
        let mut next = new_root.last_child_or_token();
        while let Some(n) = next {
            if n.kind() == WHITESPACE || n.kind() == NEWLINE {
                new_tail_len += 1;
                next = n.prev_sibling_or_token();
            } else {
                break;
            }
        }
        // Then, inherit the whitespace from the old relation
        let mut prev = old_root.first_child_or_token();
        let mut old_head = vec![];
        while let Some(p) = prev {
            if p.kind() == WHITESPACE || p.kind() == NEWLINE {
                old_head.push(p.clone());
                prev = p.next_sibling_or_token();
            } else {
                break;
            }
        }
        let mut old_tail = vec![];
        let mut next = old_root.last_child_or_token();
        while let Some(n) = next {
            if n.kind() == WHITESPACE || n.kind() == NEWLINE {
                old_tail.push(n.clone());
                next = n.prev_sibling_or_token();
            } else {
                break;
            }
        }
        new_root.splice_children(0..new_head_len, old_head);
        let tail_pos = new_root.children_with_tokens().count() - new_tail_len;
        new_root.splice_children(
            tail_pos - new_tail_len..tail_pos,
            old_tail.into_iter().rev(),
        );
        let index = old_root.index();
        self.0
            .splice_children(index..index + 1, vec![new_root.into()]);
    }

    /// Wrap and sort the relations in this entry
    #[must_use]
    pub fn wrap_and_sort(&self) -> Self {
        let mut relations = self
            .relations()
            .map(|r| r.wrap_and_sort())
            .collect::<Vec<_>>();
        // TODO: preserve comments
        relations.sort();
        Self::from(relations)
    }

    /// Iterate over the relations in this entry
    pub fn relations(&self) -> impl Iterator<Item = Relation> + '_ {
        self.0.children().filter_map(Relation::cast)
    }

    /// Iterate over the relations in this entry
    pub fn iter(&self) -> impl Iterator<Item = Relation> + '_ {
        self.relations()
    }

    /// Get the relation at the given index
    pub fn get_relation(&self, idx: usize) -> Option<Relation> {
        self.relations().nth(idx)
    }

    /// Remove the relation at the given index
    ///
    /// # Arguments
    /// * `idx` - The index of the relation to remove
    ///
    /// # Example
    /// ```
    /// use debian_control::lossless::relations::{Relation,Entry};
    /// let mut entry: Entry = r"python3-dulwich (>= 0.19.0) | python3-requests".parse().unwrap();
    /// entry.remove_relation(1);
    /// assert_eq!(entry.to_string(), "python3-dulwich (>= 0.19.0)");
    /// ```
    pub fn remove_relation(&self, idx: usize) -> Relation {
        let mut relation = self.get_relation(idx).unwrap();
        relation.remove();
        relation
    }

    /// Check if this entry is satisfied by the given package versions.
    ///
    /// # Arguments
    /// * `package_version` - A function that returns the version of a package.
    ///
    /// # Example
    /// ```
    /// use debian_control::lossless::relations::{Relation,Entry};
    /// let entry = Entry::from(vec!["samba (>= 2.0)".parse::<Relation>().unwrap()]);
    /// assert!(entry.satisfied_by(|name: &str| -> Option<debversion::Version> {
    ///    match name {
    ///    "samba" => Some("2.0".parse().unwrap()),
    ///    _ => None
    /// }}));
    /// ```
    pub fn satisfied_by(&self, package_version: impl crate::VersionLookup + Copy) -> bool {
        self.relations().any(|r| {
            let actual = package_version.lookup_version(r.name().as_str());
            if let Some((vc, version)) = r.version() {
                if let Some(actual) = actual {
                    match vc {
                        VersionConstraint::GreaterThanEqual => *actual >= version,
                        VersionConstraint::LessThanEqual => *actual <= version,
                        VersionConstraint::Equal => *actual == version,
                        VersionConstraint::GreaterThan => *actual > version,
                        VersionConstraint::LessThan => *actual < version,
                    }
                } else {
                    false
                }
            } else {
                actual.is_some()
            }
        })
    }

    /// Remove this entry
    ///
    /// # Example
    /// ```
    /// use debian_control::lossless::relations::{Relations,Entry};
    /// let mut relations: Relations = r"python3-dulwich (>= 0.19.0), python3-urllib3 (<< 1.26.0)".parse().unwrap();
    /// let mut entry = relations.get_entry(0).unwrap();
    /// entry.remove();
    /// assert_eq!(relations.to_string(), "python3-urllib3 (<< 1.26.0)");
    /// ```
    pub fn remove(&mut self) {
        let mut removed_comma = false;
        let is_first = !self
            .0
            .siblings(Direction::Prev)
            .skip(1)
            .any(|n| n.kind() == ENTRY);
        while let Some(n) = self.0.next_sibling_or_token() {
            if n.kind() == WHITESPACE || n.kind() == NEWLINE {
                n.detach();
            } else if n.kind() == COMMA {
                n.detach();
                removed_comma = true;
                break;
            } else {
                panic!("Unexpected node: {:?}", n);
            }
        }
        if !is_first {
            while let Some(n) = self.0.prev_sibling_or_token() {
                if n.kind() == WHITESPACE || n.kind() == NEWLINE {
                    n.detach();
                } else if !removed_comma && n.kind() == COMMA {
                    n.detach();
                    break;
                } else {
                    break;
                }
            }
        } else {
            while let Some(n) = self.0.next_sibling_or_token() {
                if n.kind() == WHITESPACE || n.kind() == NEWLINE {
                    n.detach();
                } else {
                    break;
                }
            }
        }
        self.0.detach();
    }

    /// Check if this entry is empty
    pub fn is_empty(&self) -> bool {
        self.relations().count() == 0
    }

    /// Get the number of relations in this entry
    pub fn len(&self) -> usize {
        self.relations().count()
    }

    /// Push a new relation to the entry
    ///
    /// # Arguments
    /// * `relation` - The relation to push
    ///
    /// # Example
    /// ```
    /// use debian_control::lossless::relations::{Relation,Entry};
    /// let mut entry: Entry = "samba (>= 2.0)".parse().unwrap();
    /// entry.push("python3-requests".parse().unwrap());
    /// assert_eq!(entry.to_string(), "samba (>= 2.0) | python3-requests");
    /// ```
    pub fn push(&mut self, relation: Relation) {
        let is_empty = !self
            .0
            .children_with_tokens()
            .any(|n| n.kind() == PIPE || n.kind() == RELATION);

        let (position, new_children) = if let Some(current_relation) = self.relations().last() {
            let to_insert: Vec<NodeOrToken<GreenNode, GreenToken>> = if is_empty {
                vec![relation.0.green().into()]
            } else {
                vec![
                    NodeOrToken::Token(GreenToken::new(WHITESPACE.into(), " ")),
                    NodeOrToken::Token(GreenToken::new(PIPE.into(), "|")),
                    NodeOrToken::Token(GreenToken::new(WHITESPACE.into(), " ")),
                    relation.0.green().into(),
                ]
            };

            (current_relation.0.index() + 1, to_insert)
        } else {
            let child_count = self.0.children_with_tokens().count();
            (
                child_count,
                if is_empty {
                    vec![relation.0.green().into()]
                } else {
                    vec![
                        NodeOrToken::Token(GreenToken::new(PIPE.into(), "|")),
                        NodeOrToken::Token(GreenToken::new(WHITESPACE.into(), " ")),
                        relation.0.green().into(),
                    ]
                },
            )
        };

        let new_root = SyntaxNode::new_root_mut(
            self.0.replace_with(
                self.0
                    .green()
                    .splice_children(position..position, new_children),
            ),
        );

        if let Some(parent) = self.0.parent() {
            parent.splice_children(self.0.index()..self.0.index() + 1, vec![new_root.into()]);
            self.0 = parent
                .children_with_tokens()
                .nth(self.0.index())
                .unwrap()
                .clone()
                .into_node()
                .unwrap();
        } else {
            self.0 = new_root;
        }
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

impl From<Vec<Relation>> for Entry {
    fn from(relations: Vec<Relation>) -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::ENTRY.into());
        for (i, relation) in relations.into_iter().enumerate() {
            if i > 0 {
                builder.token(WHITESPACE.into(), " ");
                builder.token(COMMA.into(), "|");
                builder.token(WHITESPACE.into(), " ");
            }
            inject(&mut builder, relation.0);
        }
        builder.finish_node();
        Entry(SyntaxNode::new_root_mut(builder.finish()))
    }
}

impl From<Relation> for Entry {
    fn from(relation: Relation) -> Self {
        Self::from(vec![relation])
    }
}

impl Relation {
    /// Create a new relation
    ///
    /// # Arguments
    /// * `name` - The name of the package
    /// * `version_constraint` - The version constraint and version to use
    ///
    /// # Example
    /// ```
    /// use debian_control::lossless::relations::{Relation};
    /// use debian_control::relations::VersionConstraint;
    /// let relation = Relation::new("samba", Some((VersionConstraint::GreaterThanEqual, "2.0".parse().unwrap())));
    /// assert_eq!(relation.to_string(), "samba (>= 2.0)");
    /// ```
    pub fn new(name: &str, version_constraint: Option<(VersionConstraint, Version)>) -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::RELATION.into());
        builder.token(IDENT.into(), name);
        if let Some((vc, version)) = version_constraint {
            builder.token(WHITESPACE.into(), " ");
            builder.start_node(SyntaxKind::VERSION.into());
            builder.token(L_PARENS.into(), "(");
            builder.start_node(SyntaxKind::CONSTRAINT.into());
            for c in vc.to_string().chars() {
                builder.token(
                    match c {
                        '>' => R_ANGLE.into(),
                        '<' => L_ANGLE.into(),
                        '=' => EQUAL.into(),
                        _ => unreachable!(),
                    },
                    c.to_string().as_str(),
                );
            }
            builder.finish_node();

            builder.token(WHITESPACE.into(), " ");

            builder.token(IDENT.into(), version.to_string().as_str());

            builder.token(R_PARENS.into(), ")");

            builder.finish_node();
        }

        builder.finish_node();
        Relation(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Wrap and sort this relation
    ///
    /// # Example
    /// ```
    /// use debian_control::lossless::relations::Relation;
    /// let relation = "  samba  (  >= 2.0) ".parse::<Relation>().unwrap();
    /// assert_eq!(relation.wrap_and_sort().to_string(), "samba (>= 2.0)");
    /// ```
    #[must_use]
    pub fn wrap_and_sort(&self) -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::RELATION.into());
        builder.token(IDENT.into(), self.name().as_str());
        if let Some(archqual) = self.archqual() {
            builder.token(COLON.into(), ":");
            builder.token(IDENT.into(), archqual.as_str());
        }
        if let Some((vc, version)) = self.version() {
            builder.token(WHITESPACE.into(), " ");
            builder.start_node(SyntaxKind::VERSION.into());
            builder.token(L_PARENS.into(), "(");
            builder.start_node(SyntaxKind::CONSTRAINT.into());
            builder.token(
                match vc {
                    VersionConstraint::GreaterThanEqual => R_ANGLE.into(),
                    VersionConstraint::LessThanEqual => L_ANGLE.into(),
                    VersionConstraint::Equal => EQUAL.into(),
                    VersionConstraint::GreaterThan => R_ANGLE.into(),
                    VersionConstraint::LessThan => L_ANGLE.into(),
                },
                vc.to_string().as_str(),
            );
            builder.finish_node();
            builder.token(WHITESPACE.into(), " ");
            builder.token(IDENT.into(), version.to_string().as_str());
            builder.token(R_PARENS.into(), ")");
            builder.finish_node();
        }
        if let Some(architectures) = self.architectures() {
            builder.token(WHITESPACE.into(), " ");
            builder.start_node(ARCHITECTURES.into());
            builder.token(L_BRACKET.into(), "[");
            for (i, arch) in architectures.enumerate() {
                if i > 0 {
                    builder.token(WHITESPACE.into(), " ");
                }
                builder.token(IDENT.into(), arch.as_str());
            }
            builder.token(R_BRACKET.into(), "]");
            builder.finish_node();
        }
        for profiles in self.profiles() {
            builder.token(WHITESPACE.into(), " ");
            builder.start_node(PROFILES.into());
            builder.token(L_ANGLE.into(), "<");
            for (i, profile) in profiles.into_iter().enumerate() {
                if i > 0 {
                    builder.token(WHITESPACE.into(), " ");
                }
                match profile {
                    BuildProfile::Disabled(name) => {
                        builder.token(NOT.into(), "!");
                        builder.token(IDENT.into(), name.as_str());
                    }
                    BuildProfile::Enabled(name) => {
                        builder.token(IDENT.into(), name.as_str());
                    }
                }
            }
            builder.token(R_ANGLE.into(), ">");
            builder.finish_node();
        }
        builder.finish_node();
        Relation(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Create a new simple relation, without any version constraints.
    ///
    /// # Example
    /// ```
    /// use debian_control::lossless::relations::Relation;
    /// let relation = Relation::simple("samba");
    /// assert_eq!(relation.to_string(), "samba");
    /// ```
    pub fn simple(name: &str) -> Self {
        Self::new(name, None)
    }

    /// Remove the version constraint from the relation.
    ///
    /// # Example
    /// ```
    /// use debian_control::lossless::relations::{Relation};
    /// use debian_control::relations::VersionConstraint;
    /// let mut relation = Relation::new("samba", Some((VersionConstraint::GreaterThanEqual, "2.0".parse().unwrap())));
    /// relation.drop_constraint();
    /// assert_eq!(relation.to_string(), "samba");
    /// ```
    pub fn drop_constraint(&mut self) -> bool {
        let version_token = self.0.children().find(|n| n.kind() == VERSION);
        if let Some(version_token) = version_token {
            // Remove any whitespace before the version token
            while let Some(prev) = version_token.prev_sibling_or_token() {
                if prev.kind() == WHITESPACE || prev.kind() == NEWLINE {
                    prev.detach();
                } else {
                    break;
                }
            }
            version_token.detach();
            return true;
        }

        false
    }

    /// Return the name of the package in the relation.
    ///
    /// # Example
    /// ```
    /// use debian_control::lossless::relations::Relation;
    /// let relation = Relation::simple("samba");
    /// assert_eq!(relation.name(), "samba");
    /// ```
    pub fn name(&self) -> String {
        self.0
            .children_with_tokens()
            .find_map(|it| match it {
                SyntaxElement::Token(token) if token.kind() == IDENT => Some(token),
                _ => None,
            })
            .unwrap()
            .text()
            .to_string()
    }

    /// Return the archqual
    ///
    /// # Example
    /// ```
    /// use debian_control::lossless::relations::Relation;
    /// let relation: Relation = "samba:any".parse().unwrap();
    /// assert_eq!(relation.archqual(), Some("any".to_string()));
    /// ```
    pub fn archqual(&self) -> Option<String> {
        let archqual = self.0.children().find(|n| n.kind() == ARCHQUAL);
        let node = if let Some(archqual) = archqual {
            archqual.children_with_tokens().find_map(|it| match it {
                SyntaxElement::Token(token) if token.kind() == IDENT => Some(token),
                _ => None,
            })
        } else {
            None
        };
        node.map(|n| n.text().to_string())
    }

    /// Set the architecture qualifier for this relation.
    ///
    /// # Example
    /// ```
    /// use debian_control::lossless::relations::Relation;
    /// let mut relation = Relation::simple("samba");
    /// relation.set_archqual("any");
    /// assert_eq!(relation.to_string(), "samba:any");
    /// ```
    pub fn set_archqual(&mut self, archqual: &str) {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(ARCHQUAL.into());
        builder.token(COLON.into(), ":");
        builder.token(IDENT.into(), archqual);
        builder.finish_node();

        let node_archqual = self.0.children().find(|n| n.kind() == ARCHQUAL);
        if let Some(node_archqual) = node_archqual {
            self.0.splice_children(
                node_archqual.index()..node_archqual.index() + 1,
                vec![SyntaxNode::new_root_mut(builder.finish()).into()],
            );
        } else {
            let name_node = self.0.children_with_tokens().find(|n| n.kind() == IDENT);
            let idx = if let Some(name_node) = name_node {
                name_node.index() + 1
            } else {
                0
            };
            self.0.splice_children(
                idx..idx,
                vec![SyntaxNode::new_root_mut(builder.finish()).into()],
            );
        }
    }

    /// Return the version constraint and the version it is constrained to.
    pub fn version(&self) -> Option<(VersionConstraint, Version)> {
        let vc = self.0.children().find(|n| n.kind() == VERSION);
        let vc = vc.as_ref()?;
        let constraint = vc.children().find(|n| n.kind() == CONSTRAINT);

        let version = vc.children_with_tokens().find_map(|it| match it {
            SyntaxElement::Token(token) if token.kind() == IDENT => Some(token),
            _ => None,
        });

        if let (Some(constraint), Some(version)) = (constraint, version) {
            let vc: VersionConstraint = constraint.to_string().parse().unwrap();
            Some((vc, (version.text().to_string()).parse().unwrap()))
        } else {
            None
        }
    }

    /// Set the version constraint for this relation
    ///
    /// # Example
    /// ```
    /// use debian_control::lossless::relations::{Relation};
    /// use debian_control::relations::VersionConstraint;
    /// let mut relation = Relation::simple("samba");
    /// relation.set_version(Some((VersionConstraint::GreaterThanEqual, "2.0".parse().unwrap())));
    /// assert_eq!(relation.to_string(), "samba (>= 2.0)");
    /// ```
    pub fn set_version(&mut self, version_constraint: Option<(VersionConstraint, Version)>) {
        let current_version = self.0.children().find(|n| n.kind() == VERSION);
        if let Some((vc, version)) = version_constraint {
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(VERSION.into());
            builder.token(L_PARENS.into(), "(");
            builder.start_node(CONSTRAINT.into());
            match vc {
                VersionConstraint::GreaterThanEqual => {
                    builder.token(R_ANGLE.into(), ">");
                    builder.token(EQUAL.into(), "=");
                }
                VersionConstraint::LessThanEqual => {
                    builder.token(L_ANGLE.into(), "<");
                    builder.token(EQUAL.into(), "=");
                }
                VersionConstraint::Equal => {
                    builder.token(EQUAL.into(), "=");
                }
                VersionConstraint::GreaterThan => {
                    builder.token(R_ANGLE.into(), ">");
                }
                VersionConstraint::LessThan => {
                    builder.token(L_ANGLE.into(), "<");
                }
            }
            builder.finish_node(); // CONSTRAINT
            builder.token(WHITESPACE.into(), " ");
            builder.token(IDENT.into(), version.to_string().as_str());
            builder.token(R_PARENS.into(), ")");
            builder.finish_node(); // VERSION

            if let Some(current_version) = current_version {
                self.0.splice_children(
                    current_version.index()..current_version.index() + 1,
                    vec![SyntaxNode::new_root_mut(builder.finish()).into()],
                );
            } else {
                let name_node = self.0.children_with_tokens().find(|n| n.kind() == IDENT);
                let idx = if let Some(name_node) = name_node {
                    name_node.index() + 1
                } else {
                    0
                };
                let new_children = vec![
                    GreenToken::new(WHITESPACE.into(), " ").into(),
                    builder.finish().into(),
                ];
                let new_root = SyntaxNode::new_root_mut(
                    self.0.green().splice_children(idx..idx, new_children),
                );
                if let Some(parent) = self.0.parent() {
                    parent
                        .splice_children(self.0.index()..self.0.index() + 1, vec![new_root.into()]);
                    self.0 = parent
                        .children_with_tokens()
                        .nth(self.0.index())
                        .unwrap()
                        .clone()
                        .into_node()
                        .unwrap();
                } else {
                    self.0 = new_root;
                }
            }
        } else if let Some(current_version) = current_version {
            // Remove any whitespace before the version token
            while let Some(prev) = current_version.prev_sibling_or_token() {
                if prev.kind() == WHITESPACE || prev.kind() == NEWLINE {
                    prev.detach();
                } else {
                    break;
                }
            }
            current_version.detach();
        }
    }

    /// Return an iterator over the architectures for this relation
    ///
    /// # Example
    /// ```
    /// use debian_control::lossless::relations::Relation;
    /// let relation: Relation = "samba [amd64]".parse().unwrap();
    /// assert_eq!(relation.architectures().unwrap().collect::<Vec<_>>(), vec!["amd64".to_string()]);
    /// ```
    pub fn architectures(&self) -> Option<impl Iterator<Item = String> + '_> {
        let architectures = self.0.children().find(|n| n.kind() == ARCHITECTURES)?;

        Some(architectures.children_with_tokens().filter_map(|node| {
            let token = node.as_token()?;
            if token.kind() == IDENT {
                Some(token.text().to_string())
            } else {
                None
            }
        }))
    }

    /// Returns an iterator over the build profiles for this relation
    ///
    /// # Example
    /// ```
    /// use debian_control::lossless::relations::{Relation};
    /// use debian_control::relations::{BuildProfile};
    /// let relation: Relation = "samba <!nocheck>".parse().unwrap();
    /// assert_eq!(relation.profiles().collect::<Vec<_>>(), vec![vec![BuildProfile::Disabled("nocheck".to_string())]]);
    /// ```
    pub fn profiles(&self) -> impl Iterator<Item = Vec<BuildProfile>> + '_ {
        let profiles = self.0.children().filter(|n| n.kind() == PROFILES);

        profiles.map(|profile| {
            // iterate over nodes separated by whitespace tokens
            let mut ret = vec![];
            let mut current = vec![];
            for token in profile.children_with_tokens() {
                match token.kind() {
                    WHITESPACE | NEWLINE => {
                        if !current.is_empty() {
                            ret.push(current.join("").parse::<BuildProfile>().unwrap());
                            current = vec![];
                        }
                    }
                    L_ANGLE | R_ANGLE => {}
                    _ => {
                        current.push(token.to_string());
                    }
                }
            }
            if !current.is_empty() {
                ret.push(current.concat().parse().unwrap());
            }
            ret
        })
    }

    /// Remove this relation
    ///
    /// # Example
    /// ```
    /// use debian_control::lossless::relations::{Relation,Entry};
    /// let mut entry: Entry = r"python3-dulwich (>= 0.19.0) | python3-urllib3 (<< 1.26.0)".parse().unwrap();
    /// let mut relation = entry.get_relation(0).unwrap();
    /// relation.remove();
    /// assert_eq!(entry.to_string(), "python3-urllib3 (<< 1.26.0)");
    /// ```
    pub fn remove(&mut self) {
        let is_first = !self
            .0
            .siblings(Direction::Prev)
            .skip(1)
            .any(|n| n.kind() == RELATION);
        if !is_first {
            // Not the first item in the list. Remove whitespace backwards to the previous
            // pipe, the pipe and any whitespace until the previous relation
            while let Some(n) = self.0.prev_sibling_or_token() {
                if n.kind() == WHITESPACE || n.kind() == NEWLINE {
                    n.detach();
                } else if n.kind() == PIPE {
                    n.detach();
                    break;
                } else {
                    break;
                }
            }
            while let Some(n) = self.0.prev_sibling_or_token() {
                if n.kind() == WHITESPACE || n.kind() == NEWLINE {
                    n.detach();
                } else {
                    break;
                }
            }
        } else {
            // First item in the list. Remove whitespace up to the pipe, the pipe and anything
            // before the next relation
            while let Some(n) = self.0.next_sibling_or_token() {
                if n.kind() == WHITESPACE || n.kind() == NEWLINE {
                    n.detach();
                } else if n.kind() == PIPE {
                    n.detach();
                    break;
                } else {
                    panic!("Unexpected node: {:?}", n);
                }
            }

            while let Some(n) = self.0.next_sibling_or_token() {
                if n.kind() == WHITESPACE || n.kind() == NEWLINE {
                    n.detach();
                } else {
                    break;
                }
            }
        }
        // If this was the last relation in the entry, remove the entire entry
        if let Some(mut parent) = self.0.parent().and_then(Entry::cast) {
            if parent.is_empty() {
                parent.remove();
            } else {
                self.0.detach();
            }
        } else {
            self.0.detach();
        }
    }

    /// Set the architectures for this relation
    ///
    /// # Example
    /// ```
    /// use debian_control::lossless::relations::Relation;
    /// let mut relation = Relation::simple("samba");
    /// relation.set_architectures(vec!["amd64", "i386"].into_iter());
    /// assert_eq!(relation.to_string(), "samba [amd64 i386]");
    /// ```
    pub fn set_architectures<'a>(&mut self, architectures: impl Iterator<Item = &'a str>) {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(ARCHITECTURES.into());
        builder.token(L_BRACKET.into(), "[");
        for (i, arch) in architectures.enumerate() {
            if i > 0 {
                builder.token(WHITESPACE.into(), " ");
            }
            builder.token(IDENT.into(), arch);
        }
        builder.token(R_BRACKET.into(), "]");
        builder.finish_node();

        let node_architectures = self.0.children().find(|n| n.kind() == ARCHITECTURES);
        if let Some(node_architectures) = node_architectures {
            let new_root = SyntaxNode::new_root_mut(builder.finish());
            self.0.splice_children(
                node_architectures.index()..node_architectures.index() + 1,
                vec![new_root.into()],
            );
        } else {
            let profiles = self.0.children().find(|n| n.kind() == PROFILES);
            let idx = if let Some(profiles) = profiles {
                profiles.index()
            } else {
                self.0.children_with_tokens().count()
            };
            let new_root = SyntaxNode::new_root(self.0.green().splice_children(
                idx..idx,
                vec![
                    GreenToken::new(WHITESPACE.into(), " ").into(),
                    builder.finish().into(),
                ],
            ));
            if let Some(parent) = self.0.parent() {
                parent.splice_children(self.0.index()..self.0.index() + 1, vec![new_root.into()]);
                self.0 = parent
                    .children_with_tokens()
                    .nth(self.0.index())
                    .unwrap()
                    .clone()
                    .into_node()
                    .unwrap();
            } else {
                self.0 = new_root;
            }
        }
    }

    /// Add a build profile to this relation
    ///
    /// # Example
    /// ```
    /// use debian_control::lossless::relations::Relation;
    /// use debian_control::relations::BuildProfile;
    /// let mut relation = Relation::simple("samba");
    /// relation.add_profile(&[BuildProfile::Disabled("nocheck".to_string())]);
    /// assert_eq!(relation.to_string(), "samba <!nocheck>");
    /// ```
    pub fn add_profile(&mut self, profile: &[BuildProfile]) {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(PROFILES.into());
        builder.token(L_ANGLE.into(), "<");
        for (i, profile) in profile.iter().enumerate() {
            if i > 0 {
                builder.token(WHITESPACE.into(), " ");
            }
            match profile {
                BuildProfile::Disabled(name) => {
                    builder.token(NOT.into(), "!");
                    builder.token(IDENT.into(), name.as_str());
                }
                BuildProfile::Enabled(name) => {
                    builder.token(IDENT.into(), name.as_str());
                }
            }
        }
        builder.token(R_ANGLE.into(), ">");
        builder.finish_node();

        let node_profiles = self.0.children().find(|n| n.kind() == PROFILES);
        if let Some(node_profiles) = node_profiles {
            let new_root = SyntaxNode::new_root_mut(builder.finish());
            self.0.splice_children(
                node_profiles.index()..node_profiles.index() + 1,
                vec![new_root.into()],
            );
        } else {
            let idx = self.0.children_with_tokens().count();
            let new_root = SyntaxNode::new_root(self.0.green().splice_children(
                idx..idx,
                vec![
                    GreenToken::new(WHITESPACE.into(), " ").into(),
                    builder.finish().into(),
                ],
            ));
            if let Some(parent) = self.0.parent() {
                parent.splice_children(self.0.index()..self.0.index() + 1, vec![new_root.into()]);
                self.0 = parent
                    .children_with_tokens()
                    .nth(self.0.index())
                    .unwrap()
                    .clone()
                    .into_node()
                    .unwrap();
            } else {
                self.0 = new_root;
            }
        }
    }

    /// Build a new relation
    pub fn build(name: &str) -> RelationBuilder {
        RelationBuilder::new(name)
    }
}

/// A builder for creating a `Relation`
///
/// # Example
/// ```
/// use debian_control::lossless::relations::{Relation};
/// use debian_control::relations::VersionConstraint;
/// let relation = Relation::build("samba")
///    .version_constraint(VersionConstraint::GreaterThanEqual, "2.0".parse().unwrap())
///    .archqual("any")
///    .architectures(vec!["amd64".to_string(), "i386".to_string()])
///    .build();
/// assert_eq!(relation.to_string(), "samba:any (>= 2.0) [amd64 i386]");
/// ```
pub struct RelationBuilder {
    name: String,
    version_constraint: Option<(VersionConstraint, Version)>,
    archqual: Option<String>,
    architectures: Vec<String>,
    profiles: Vec<Vec<BuildProfile>>,
}

impl RelationBuilder {
    /// Create a new `RelationBuilder` with the given package name
    fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            version_constraint: None,
            archqual: None,
            architectures: vec![],
            profiles: vec![],
        }
    }

    /// Set the version constraint for this relation
    pub fn version_constraint(mut self, vc: VersionConstraint, version: Version) -> Self {
        self.version_constraint = Some((vc, version));
        self
    }

    /// Set the architecture qualifier for this relation
    pub fn archqual(mut self, archqual: &str) -> Self {
        self.archqual = Some(archqual.to_string());
        self
    }

    /// Set the architectures for this relation
    pub fn architectures(mut self, architectures: Vec<String>) -> Self {
        self.architectures = architectures;
        self
    }

    /// Set the build profiles for this relation
    pub fn profiles(mut self, profiles: Vec<Vec<BuildProfile>>) -> Self {
        self.profiles = profiles;
        self
    }

    /// Add a build profile to this relation
    pub fn add_profile(mut self, profile: Vec<BuildProfile>) -> Self {
        self.profiles.push(profile);
        self
    }

    /// Build the `Relation`
    pub fn build(self) -> Relation {
        let mut relation = Relation::new(&self.name, self.version_constraint);
        if let Some(archqual) = &self.archqual {
            relation.set_archqual(archqual);
        }
        relation.set_architectures(self.architectures.iter().map(|s| s.as_str()));
        for profile in &self.profiles {
            relation.add_profile(profile);
        }
        relation
    }
}

impl PartialOrd for Relation {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // Compare by name first, then by version
        let name_cmp = self.name().cmp(&other.name());
        if name_cmp != std::cmp::Ordering::Equal {
            return Some(name_cmp);
        }

        let self_version = self.version();
        let other_version = other.version();

        match (self_version, other_version) {
            (Some((self_vc, self_version)), Some((other_vc, other_version))) => {
                let vc_cmp = self_vc.cmp(&other_vc);
                if vc_cmp != std::cmp::Ordering::Equal {
                    return Some(vc_cmp);
                }

                Some(self_version.cmp(&other_version))
            }
            (Some(_), None) => Some(std::cmp::Ordering::Greater),
            (None, Some(_)) => Some(std::cmp::Ordering::Less),
            (None, None) => Some(std::cmp::Ordering::Equal),
        }
    }
}

impl Eq for Relation {}

impl Ord for Relation {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl std::str::FromStr for Relations {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parse = parse(s, false);
        if parse.errors.is_empty() {
            Ok(parse.root_mut())
        } else {
            Err(parse.errors.join("\n"))
        }
    }
}

impl std::str::FromStr for Entry {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let root: Relations = s.parse()?;

        let mut entries = root.entries();
        let entry = if let Some(entry) = entries.next() {
            entry
        } else {
            return Err("No entry found".to_string());
        };

        if entries.next().is_some() {
            return Err("Multiple entries found".to_string());
        }

        Ok(entry)
    }
}

impl std::str::FromStr for Relation {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let entry: Entry = s.parse()?;

        let mut relations = entry.relations();
        let relation = if let Some(relation) = relations.next() {
            relation
        } else {
            return Err("No relation found".to_string());
        };

        if relations.next().is_some() {
            return Err("Multiple relations found".to_string());
        }

        Ok(relation)
    }
}

impl From<crate::lossy::Relation> for Relation {
    fn from(relation: crate::lossy::Relation) -> Self {
        let mut builder = Relation::build(&relation.name);

        if let Some((vc, version)) = relation.version {
            builder = builder.version_constraint(vc, version);
        }

        if let Some(archqual) = relation.archqual {
            builder = builder.archqual(&archqual);
        }

        if let Some(architectures) = relation.architectures {
            builder = builder.architectures(architectures);
        }

        builder = builder.profiles(relation.profiles);

        builder.build()
    }
}

impl From<Relation> for crate::lossy::Relation {
    fn from(relation: Relation) -> Self {
        crate::lossy::Relation {
            name: relation.name(),
            version: relation.version(),
            archqual: relation.archqual(),
            architectures: relation.architectures().map(|a| a.collect()),
            profiles: relation.profiles().collect(),
        }
    }
}

impl From<Entry> for Vec<crate::lossy::Relation> {
    fn from(entry: Entry) -> Self {
        entry.relations().map(|r| r.into()).collect()
    }
}

impl From<Vec<crate::lossy::Relation>> for Entry {
    fn from(relations: Vec<crate::lossy::Relation>) -> Self {
        let relations: Vec<Relation> = relations.into_iter().map(|r| r.into()).collect();
        Entry::from(relations)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let input = "python3-dulwich";
        let parsed: Relations = input.parse().unwrap();
        assert_eq!(parsed.to_string(), input);
        assert_eq!(parsed.entries().count(), 1);
        let entry = parsed.entries().next().unwrap();
        assert_eq!(entry.to_string(), "python3-dulwich");
        assert_eq!(entry.relations().count(), 1);
        let relation = entry.relations().next().unwrap();
        assert_eq!(relation.to_string(), "python3-dulwich");
        assert_eq!(relation.version(), None);

        let input = "python3-dulwich (>= 0.20.21)";
        let parsed: Relations = input.parse().unwrap();
        assert_eq!(parsed.to_string(), input);
        assert_eq!(parsed.entries().count(), 1);
        let entry = parsed.entries().next().unwrap();
        assert_eq!(entry.to_string(), "python3-dulwich (>= 0.20.21)");
        assert_eq!(entry.relations().count(), 1);
        let relation = entry.relations().next().unwrap();
        assert_eq!(relation.to_string(), "python3-dulwich (>= 0.20.21)");
        assert_eq!(
            relation.version(),
            Some((
                VersionConstraint::GreaterThanEqual,
                "0.20.21".parse().unwrap()
            ))
        );
    }

    #[test]
    fn test_multiple() {
        let input = "python3-dulwich (>= 0.20.21), python3-dulwich (<< 0.21)";
        let parsed: Relations = input.parse().unwrap();
        assert_eq!(parsed.to_string(), input);
        assert_eq!(parsed.entries().count(), 2);
        let entry = parsed.entries().next().unwrap();
        assert_eq!(entry.to_string(), "python3-dulwich (>= 0.20.21)");
        assert_eq!(entry.relations().count(), 1);
        let relation = entry.relations().next().unwrap();
        assert_eq!(relation.to_string(), "python3-dulwich (>= 0.20.21)");
        assert_eq!(
            relation.version(),
            Some((
                VersionConstraint::GreaterThanEqual,
                "0.20.21".parse().unwrap()
            ))
        );
        let entry = parsed.entries().nth(1).unwrap();
        assert_eq!(entry.to_string(), "python3-dulwich (<< 0.21)");
        assert_eq!(entry.relations().count(), 1);
        let relation = entry.relations().next().unwrap();
        assert_eq!(relation.to_string(), "python3-dulwich (<< 0.21)");
        assert_eq!(
            relation.version(),
            Some((VersionConstraint::LessThan, "0.21".parse().unwrap()))
        );
    }

    #[test]
    fn test_architectures() {
        let input = "python3-dulwich [amd64 arm64 armhf i386 mips mips64el mipsel ppc64el s390x]";
        let parsed: Relations = input.parse().unwrap();
        assert_eq!(parsed.to_string(), input);
        assert_eq!(parsed.entries().count(), 1);
        let entry = parsed.entries().next().unwrap();
        assert_eq!(
            entry.to_string(),
            "python3-dulwich [amd64 arm64 armhf i386 mips mips64el mipsel ppc64el s390x]"
        );
        assert_eq!(entry.relations().count(), 1);
        let relation = entry.relations().next().unwrap();
        assert_eq!(
            relation.to_string(),
            "python3-dulwich [amd64 arm64 armhf i386 mips mips64el mipsel ppc64el s390x]"
        );
        assert_eq!(relation.version(), None);
        assert_eq!(
            relation.architectures().unwrap().collect::<Vec<_>>(),
            vec![
                "amd64", "arm64", "armhf", "i386", "mips", "mips64el", "mipsel", "ppc64el", "s390x"
            ]
            .into_iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_profiles() {
        let input = "foo (>= 1.0) [i386 arm] <!nocheck> <!cross>, bar";
        let parsed: Relations = input.parse().unwrap();
        assert_eq!(parsed.to_string(), input);
        assert_eq!(parsed.entries().count(), 2);
        let entry = parsed.entries().next().unwrap();
        assert_eq!(
            entry.to_string(),
            "foo (>= 1.0) [i386 arm] <!nocheck> <!cross>"
        );
        assert_eq!(entry.relations().count(), 1);
        let relation = entry.relations().next().unwrap();
        assert_eq!(
            relation.to_string(),
            "foo (>= 1.0) [i386 arm] <!nocheck> <!cross>"
        );
        assert_eq!(
            relation.version(),
            Some((VersionConstraint::GreaterThanEqual, "1.0".parse().unwrap()))
        );
        assert_eq!(
            relation.architectures().unwrap().collect::<Vec<_>>(),
            vec!["i386", "arm"]
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
        );
        assert_eq!(
            relation.profiles().collect::<Vec<_>>(),
            vec![
                vec![BuildProfile::Disabled("nocheck".to_string())],
                vec![BuildProfile::Disabled("cross".to_string())]
            ]
        );
    }

    #[test]
    fn test_substvar() {
        let input = "${shlibs:Depends}";

        let (parsed, errors) = Relations::parse_relaxed(input, true);
        assert_eq!(errors, Vec::<String>::new());
        assert_eq!(parsed.to_string(), input);
        assert_eq!(parsed.entries().count(), 0);

        assert_eq!(
            parsed.substvars().collect::<Vec<_>>(),
            vec!["${shlibs:Depends}"]
        );
    }

    #[test]
    fn test_new() {
        let r = Relation::new(
            "samba",
            Some((VersionConstraint::GreaterThanEqual, "2.0".parse().unwrap())),
        );

        assert_eq!(r.to_string(), "samba (>= 2.0)");
    }

    #[test]
    fn test_drop_constraint() {
        let mut r = Relation::new(
            "samba",
            Some((VersionConstraint::GreaterThanEqual, "2.0".parse().unwrap())),
        );

        r.drop_constraint();

        assert_eq!(r.to_string(), "samba");
    }

    #[test]
    fn test_simple() {
        let r = Relation::simple("samba");

        assert_eq!(r.to_string(), "samba");
    }

    #[test]
    fn test_remove_first_entry() {
        let mut rels: Relations = r#"python3-dulwich (>= 0.20.21), python3-dulwich (<< 0.21)"#
            .parse()
            .unwrap();
        let removed = rels.remove_entry(0);
        assert_eq!(removed.to_string(), "python3-dulwich (>= 0.20.21)");
        assert_eq!(rels.to_string(), "python3-dulwich (<< 0.21)");
    }

    #[test]
    fn test_remove_last_entry() {
        let mut rels: Relations = r#"python3-dulwich (>= 0.20.21), python3-dulwich (<< 0.21)"#
            .parse()
            .unwrap();
        rels.remove_entry(1);
        assert_eq!(rels.to_string(), "python3-dulwich (>= 0.20.21)");
    }

    #[test]
    fn test_remove_middle() {
        let mut rels: Relations =
            r#"python3-dulwich (>= 0.20.21), python3-dulwich (<< 0.21), python3-dulwich (<< 0.22)"#
                .parse()
                .unwrap();
        rels.remove_entry(1);
        assert_eq!(
            rels.to_string(),
            "python3-dulwich (>= 0.20.21), python3-dulwich (<< 0.22)"
        );
    }

    #[test]
    fn test_remove_added() {
        let mut rels: Relations = r#"python3-dulwich (>= 0.20.21)"#.parse().unwrap();
        let entry = Entry::from(vec![Relation::simple("python3-dulwich")]);
        rels.push(entry);
        rels.remove_entry(1);
        assert_eq!(rels.to_string(), "python3-dulwich (>= 0.20.21)");
    }

    #[test]
    fn test_push() {
        let mut rels: Relations = r#"python3-dulwich (>= 0.20.21)"#.parse().unwrap();
        let entry = Entry::from(vec![Relation::simple("python3-dulwich")]);
        rels.push(entry);
        assert_eq!(
            rels.to_string(),
            "python3-dulwich (>= 0.20.21), python3-dulwich"
        );
    }

    #[test]
    fn test_push_from_empty() {
        let mut rels: Relations = "".parse().unwrap();
        let entry = Entry::from(vec![Relation::simple("python3-dulwich")]);
        rels.push(entry);
        assert_eq!(rels.to_string(), "python3-dulwich");
    }

    #[test]
    fn test_insert() {
        let mut rels: Relations = r#"python3-dulwich (>= 0.20.21), python3-dulwich (<< 0.21)"#
            .parse()
            .unwrap();
        let entry = Entry::from(vec![Relation::simple("python3-dulwich")]);
        rels.insert(1, entry);
        assert_eq!(
            rels.to_string(),
            "python3-dulwich (>= 0.20.21), python3-dulwich, python3-dulwich (<< 0.21)"
        );
    }

    #[test]
    fn test_insert_at_start() {
        let mut rels: Relations = r#"python3-dulwich (>= 0.20.21), python3-dulwich (<< 0.21)"#
            .parse()
            .unwrap();
        let entry = Entry::from(vec![Relation::simple("python3-dulwich")]);
        rels.insert(0, entry);
        assert_eq!(
            rels.to_string(),
            "python3-dulwich, python3-dulwich (>= 0.20.21), python3-dulwich (<< 0.21)"
        );
    }

    #[test]
    fn test_insert_after_error() {
        let (mut rels, errors) = Relations::parse_relaxed("@foo@, debhelper (>= 1.0)", false);
        assert_eq!(
            errors,
            vec![
                "expected $ or identifier but got ERROR",
                "expected comma or end of file but got Some(IDENT)",
                "expected $ or identifier but got ERROR"
            ]
        );
        let entry = Entry::from(vec![Relation::simple("bar")]);
        rels.push(entry);
        assert_eq!(rels.to_string(), "@foo@, debhelper (>= 1.0), bar");
    }

    #[test]
    fn test_insert_before_error() {
        let (mut rels, errors) = Relations::parse_relaxed("debhelper (>= 1.0), @foo@, bla", false);
        assert_eq!(
            errors,
            vec![
                "expected $ or identifier but got ERROR",
                "expected comma or end of file but got Some(IDENT)",
                "expected $ or identifier but got ERROR"
            ]
        );
        let entry = Entry::from(vec![Relation::simple("bar")]);
        rels.insert(0, entry);
        assert_eq!(rels.to_string(), "bar, debhelper (>= 1.0), @foo@, bla");
    }

    #[test]
    fn test_replace() {
        let mut rels: Relations = r#"python3-dulwich (>= 0.20.21), python3-dulwich (<< 0.21)"#
            .parse()
            .unwrap();
        let entry = Entry::from(vec![Relation::simple("python3-dulwich")]);
        rels.replace(1, entry);
        assert_eq!(
            rels.to_string(),
            "python3-dulwich (>= 0.20.21), python3-dulwich"
        );
    }

    #[test]
    fn test_relation_from_entries() {
        let entries = vec![
            Entry::from(vec![Relation::simple("python3-dulwich")]),
            Entry::from(vec![Relation::simple("python3-breezy")]),
        ];
        let rels: Relations = entries.into();
        assert_eq!(rels.entries().count(), 2);
        assert_eq!(rels.to_string(), "python3-dulwich, python3-breezy");
    }

    #[test]
    fn test_entry_from_relations() {
        let relations = vec![
            Relation::simple("python3-dulwich"),
            Relation::simple("python3-breezy"),
        ];
        let entry: Entry = relations.into();
        assert_eq!(entry.relations().count(), 2);
        assert_eq!(entry.to_string(), "python3-dulwich | python3-breezy");
    }

    #[test]
    fn test_parse_entry() {
        let parsed: Entry = "python3-dulwich (>= 0.20.21) | bar".parse().unwrap();
        assert_eq!(parsed.to_string(), "python3-dulwich (>= 0.20.21) | bar");
        assert_eq!(parsed.relations().count(), 2);

        assert_eq!(
            "foo, bar".parse::<Entry>().unwrap_err(),
            "Multiple entries found"
        );
        assert_eq!("".parse::<Entry>().unwrap_err(), "No entry found");
    }

    #[test]
    fn test_parse_relation() {
        let parsed: Relation = "python3-dulwich (>= 0.20.21)".parse().unwrap();
        assert_eq!(parsed.to_string(), "python3-dulwich (>= 0.20.21)");
        assert_eq!(
            parsed.version(),
            Some((
                VersionConstraint::GreaterThanEqual,
                "0.20.21".parse().unwrap()
            ))
        );
        assert_eq!(
            "foo | bar".parse::<Relation>().unwrap_err(),
            "Multiple relations found"
        );
        assert_eq!("".parse::<Relation>().unwrap_err(), "No entry found");
    }

    #[test]
    fn test_special() {
        let parsed: Relation = "librust-breezyshim+dirty-tracker-dev:amd64 (>= 0.1.138-~~)"
            .parse()
            .unwrap();
        assert_eq!(
            parsed.to_string(),
            "librust-breezyshim+dirty-tracker-dev:amd64 (>= 0.1.138-~~)"
        );
        assert_eq!(
            parsed.version(),
            Some((
                VersionConstraint::GreaterThanEqual,
                "0.1.138-~~".parse().unwrap()
            ))
        );
        assert_eq!(parsed.archqual(), Some("amd64".to_string()));
        assert_eq!(parsed.name(), "librust-breezyshim+dirty-tracker-dev");
    }

    #[test]
    fn test_relations_satisfied_by() {
        let rels: Relations = "python3-dulwich (>= 0.20.21), python3-dulwich (<< 0.21)"
            .parse()
            .unwrap();
        let satisfied = |name: &str| -> Option<debversion::Version> {
            match name {
                "python3-dulwich" => Some("0.20.21".parse().unwrap()),
                _ => None,
            }
        };
        assert!(rels.satisfied_by(satisfied));

        let satisfied = |name: &str| match name {
            "python3-dulwich" => Some("0.21".parse().unwrap()),
            _ => None,
        };
        assert!(!rels.satisfied_by(satisfied));

        let satisfied = |name: &str| match name {
            "python3-dulwich" => Some("0.20.20".parse().unwrap()),
            _ => None,
        };
        assert!(!rels.satisfied_by(satisfied));
    }

    #[test]
    fn test_entry_satisfied_by() {
        let entry: Entry = "python3-dulwich (>= 0.20.21) | python3-dulwich (<< 0.18)"
            .parse()
            .unwrap();
        let satisfied = |name: &str| -> Option<debversion::Version> {
            match name {
                "python3-dulwich" => Some("0.20.21".parse().unwrap()),
                _ => None,
            }
        };
        assert!(entry.satisfied_by(satisfied));
        let satisfied = |name: &str| -> Option<debversion::Version> {
            match name {
                "python3-dulwich" => Some("0.18".parse().unwrap()),
                _ => None,
            }
        };
        assert!(!entry.satisfied_by(satisfied));
    }

    #[test]
    fn test_wrap_and_sort_relation() {
        let relation: Relation = "   python3-dulwich   (>= 11) [  amd64 ] <  lala>"
            .parse()
            .unwrap();

        let wrapped = relation.wrap_and_sort();

        assert_eq!(
            wrapped.to_string(),
            "python3-dulwich (>= 11) [amd64] <lala>"
        );
    }

    #[test]
    fn test_wrap_and_sort_relations() {
        let entry: Relations =
            "python3-dulwich (>= 0.20.21)   | bar, \n\n\n\npython3-dulwich (<< 0.21)"
                .parse()
                .unwrap();

        let wrapped = entry.wrap_and_sort();

        assert_eq!(
            wrapped.to_string(),
            "bar | python3-dulwich (>= 0.20.21), python3-dulwich (<< 0.21)"
        );
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_serialize_relations() {
        let relations: Relations = "python3-dulwich (>= 0.20.21), python3-dulwich (<< 0.21)"
            .parse()
            .unwrap();
        let serialized = serde_json::to_string(&relations).unwrap();
        assert_eq!(
            serialized,
            r#""python3-dulwich (>= 0.20.21), python3-dulwich (<< 0.21)""#
        );
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_deserialize_relations() {
        let relations: Relations = "python3-dulwich (>= 0.20.21), python3-dulwich (<< 0.21)"
            .parse()
            .unwrap();
        let serialized = serde_json::to_string(&relations).unwrap();
        let deserialized: Relations = serde_json::from_str(&serialized).unwrap();
        assert_eq!(deserialized.to_string(), relations.to_string());
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_serialize_relation() {
        let relation: Relation = "python3-dulwich (>= 0.20.21)".parse().unwrap();
        let serialized = serde_json::to_string(&relation).unwrap();
        assert_eq!(serialized, r#""python3-dulwich (>= 0.20.21)""#);
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_deserialize_relation() {
        let relation: Relation = "python3-dulwich (>= 0.20.21)".parse().unwrap();
        let serialized = serde_json::to_string(&relation).unwrap();
        let deserialized: Relation = serde_json::from_str(&serialized).unwrap();
        assert_eq!(deserialized.to_string(), relation.to_string());
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_serialize_entry() {
        let entry: Entry = "python3-dulwich (>= 0.20.21) | python3-dulwich (<< 0.18)"
            .parse()
            .unwrap();
        let serialized = serde_json::to_string(&entry).unwrap();
        assert_eq!(
            serialized,
            r#""python3-dulwich (>= 0.20.21) | python3-dulwich (<< 0.18)""#
        );
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_deserialize_entry() {
        let entry: Entry = "python3-dulwich (>= 0.20.21) | python3-dulwich (<< 0.18)"
            .parse()
            .unwrap();
        let serialized = serde_json::to_string(&entry).unwrap();
        let deserialized: Entry = serde_json::from_str(&serialized).unwrap();
        assert_eq!(deserialized.to_string(), entry.to_string());
    }

    #[test]
    fn test_remove_first_relation() {
        let entry: Entry = "python3-dulwich (>= 0.20.21) | python3-dulwich (<< 0.18)"
            .parse()
            .unwrap();
        let mut rel = entry.relations().next().unwrap();
        rel.remove();
        assert_eq!(entry.to_string(), "python3-dulwich (<< 0.18)");
    }

    #[test]
    fn test_remove_last_relation() {
        let entry: Entry = "python3-dulwich (>= 0.20.21) | python3-dulwich (<< 0.18)"
            .parse()
            .unwrap();
        let mut rel = entry.relations().nth(1).unwrap();
        rel.remove();
        assert_eq!(entry.to_string(), "python3-dulwich (>= 0.20.21)");
    }

    #[test]
    fn test_remove_only_relation() {
        let entry: Entry = "python3-dulwich (>= 0.20.21)".parse().unwrap();
        let mut rel = entry.relations().next().unwrap();
        rel.remove();
        assert_eq!(entry.to_string(), "");
    }

    #[test]
    fn test_relations_is_empty() {
        let entry: Relations = "python3-dulwich (>= 0.20.21)".parse().unwrap();
        assert!(!entry.is_empty());
        assert_eq!(1, entry.len());
        let mut rel = entry.entries().next().unwrap();
        rel.remove();
        assert!(entry.is_empty());
        assert_eq!(0, entry.len());
    }

    #[test]
    fn test_entry_is_empty() {
        let entry: Entry = "python3-dulwich (>= 0.20.21) | python3-dulwich (<< 0.18)"
            .parse()
            .unwrap();
        assert!(!entry.is_empty());
        assert_eq!(2, entry.len());
        let mut rel = entry.relations().next().unwrap();
        rel.remove();
        assert!(!entry.is_empty());
        assert_eq!(1, entry.len());
        let mut rel = entry.relations().next().unwrap();
        rel.remove();
        assert!(entry.is_empty());
        assert_eq!(0, entry.len());
    }

    #[test]
    fn test_relation_set_version() {
        let mut rel: Relation = "samba".parse().unwrap();
        rel.set_version(None);
        assert_eq!("samba", rel.to_string());
        rel.set_version(Some((
            VersionConstraint::GreaterThanEqual,
            "2.0".parse().unwrap(),
        )));
        assert_eq!("samba (>= 2.0)", rel.to_string());
        rel.set_version(None);
        assert_eq!("samba", rel.to_string());
        rel.set_version(Some((
            VersionConstraint::GreaterThanEqual,
            "2.0".parse().unwrap(),
        )));
        rel.set_version(Some((
            VersionConstraint::GreaterThanEqual,
            "1.1".parse().unwrap(),
        )));
        assert_eq!("samba (>= 1.1)", rel.to_string());
    }

    #[test]
    fn test_replace_relation() {
        let mut entry: Entry = "python3-dulwich (>= 0.20.21) | python3-dulwich (<< 0.18)"
            .parse()
            .unwrap();
        let new_rel = Relation::simple("python3-breezy");
        entry.replace(0, new_rel);
        assert_eq!(
            entry.to_string(),
            "python3-breezy | python3-dulwich (<< 0.18)"
        );
    }

    #[test]
    fn test_entry_push_relation() {
        let relations: Relations = "python3-dulwich (>= 0.20.21)".parse().unwrap();
        let new_rel = Relation::simple("python3-breezy");
        let mut entry = relations.entries().next().unwrap();
        entry.push(new_rel);
        assert_eq!(
            entry.to_string(),
            "python3-dulwich (>= 0.20.21) | python3-breezy"
        );
        assert_eq!(
            relations.to_string(),
            "python3-dulwich (>= 0.20.21) | python3-breezy"
        );
    }

    #[test]
    fn test_relations_remove_empty_entry() {
        let (mut relations, errors) = Relations::parse_relaxed("foo, , bar, ", false);
        assert_eq!(errors, Vec::<String>::new());
        assert_eq!(relations.to_string(), "foo, , bar, ");
        assert_eq!(relations.len(), 2);
        assert_eq!(
            relations.entries().next().unwrap().to_string(),
            "foo".to_string()
        );
        assert_eq!(
            relations.entries().nth(1).unwrap().to_string(),
            "bar".to_string()
        );
        relations.remove_entry(1);
        assert_eq!(relations.to_string(), "foo, , ");
    }

    #[test]
    fn test_entry_remove_relation() {
        let entry: Entry = "python3-dulwich | samba".parse().unwrap();
        let removed = entry.remove_relation(0);
        assert_eq!(removed.to_string(), "python3-dulwich");
        assert_eq!(entry.to_string(), "samba");
    }

    #[test]
    fn test_wrap_and_sort_removes_empty_entries() {
        let relations: Relations = "foo, , bar, ".parse().unwrap();
        let wrapped = relations.wrap_and_sort();
        assert_eq!(wrapped.to_string(), "bar, foo");
    }

    #[test]
    fn test_set_archqual() {
        let entry: Entry = "python3-dulwich | samba".parse().unwrap();
        let mut rel = entry.relations().next().unwrap();
        rel.set_archqual("amd64");
        assert_eq!(rel.to_string(), "python3-dulwich:amd64");
        assert_eq!(rel.archqual(), Some("amd64".to_string()));
        assert_eq!(entry.to_string(), "python3-dulwich:amd64 | samba");
        rel.set_archqual("i386");
        assert_eq!(rel.to_string(), "python3-dulwich:i386");
        assert_eq!(rel.archqual(), Some("i386".to_string()));
        assert_eq!(entry.to_string(), "python3-dulwich:i386 | samba");
    }

    #[test]
    fn test_set_architectures() {
        let mut relation = Relation::simple("samba");
        relation.set_architectures(vec!["amd64", "i386"].into_iter());
        assert_eq!(relation.to_string(), "samba [amd64 i386]");
    }
}
