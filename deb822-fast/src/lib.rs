//! Lossy parser for deb822 format.
//!
//! This parser is lossy in the sense that it will discard whitespace and comments
//! in the input.
use crate::lex::SyntaxKind;

#[cfg(feature = "derive")]
pub use deb822_derive::{FromDeb822, ToDeb822};

pub mod convert;
pub use convert::{FromDeb822Paragraph, ToDeb822Paragraph};
mod lex;

/// Canonical field order for source paragraphs in debian/control files
pub const SOURCE_FIELD_ORDER: &[&str] = &[
    "Source",
    "Section",
    "Priority",
    "Maintainer",
    "Uploaders",
    "Build-Depends",
    "Build-Depends-Indep",
    "Build-Depends-Arch",
    "Build-Conflicts",
    "Build-Conflicts-Indep",
    "Build-Conflicts-Arch",
    "Standards-Version",
    "Vcs-Browser",
    "Vcs-Git",
    "Vcs-Svn",
    "Vcs-Bzr",
    "Vcs-Hg",
    "Vcs-Darcs",
    "Vcs-Cvs",
    "Vcs-Arch",
    "Vcs-Mtn",
    "Homepage",
    "Rules-Requires-Root",
    "Testsuite",
    "Testsuite-Triggers",
];

/// Canonical field order for binary packages in debian/control files
pub const BINARY_FIELD_ORDER: &[&str] = &[
    "Package",
    "Architecture",
    "Section",
    "Priority",
    "Multi-Arch",
    "Essential",
    "Build-Profiles",
    "Built-Using",
    "Pre-Depends",
    "Depends",
    "Recommends",
    "Suggests",
    "Enhances",
    "Conflicts",
    "Breaks",
    "Replaces",
    "Provides",
    "Description",
];

/// Error type for the parser.
#[derive(Debug)]
pub enum Error {
    /// An unexpected token was encountered.
    UnexpectedToken(SyntaxKind, String),

    /// Unexpected end-of-file.
    UnexpectedEof,

    /// Expected end-of-file.
    ExpectedEof,

    /// IO error.
    Io(std::io::Error),
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::UnexpectedToken(_k, t) => write!(f, "Unexpected token: {}", t),
            Self::UnexpectedEof => f.write_str("Unexpected end-of-file"),
            Self::Io(e) => write!(f, "IO error: {}", e),
            Self::ExpectedEof => f.write_str("Expected end-of-file"),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io(e) => Some(e),
            _ => None,
        }
    }
}

/// A field in a deb822 paragraph.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Field {
    /// The name of the field.
    pub name: String,

    /// The value of the field.
    pub value: String,
}

/// A deb822 paragraph.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Paragraph {
    /// Fields in the paragraph.
    pub fields: Vec<Field>,
}

impl Paragraph {
    /// Get the value of a field by name.
    ///
    /// Returns `None` if the field does not exist.
    pub fn get(&self, name: &str) -> Option<&str> {
        for field in &self.fields {
            if field.name == name {
                return Some(&field.value);
            }
        }
        None
    }

    /// Check if the paragraph is empty.
    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }

    /// Return the number of fields in the paragraph.
    pub fn len(&self) -> usize {
        self.fields.len()
    }

    /// Iterate over the fields in the paragraph.
    pub fn iter(&self) -> impl Iterator<Item = (&str, &str)> {
        self.fields
            .iter()
            .map(|field| (field.name.as_str(), field.value.as_str()))
    }

    /// Iterate over the fields in the paragraph, mutably.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&str, &mut String)> {
        self.fields
            .iter_mut()
            .map(|field| (field.name.as_str(), &mut field.value))
    }

    /// Insert a field into the paragraph.
    ///
    /// If a field with the same name already exists, a
    /// new field will be added.
    pub fn insert(&mut self, name: &str, value: &str) {
        self.fields.push(Field {
            name: name.to_string(),
            value: value.to_string(),
        });
    }

    /// Set the value of a field, inserting at the appropriate location if new.
    ///
    /// If a field with the same name already exists, its value will be updated.
    /// If the field doesn't exist, it will be inserted at the appropriate position
    /// based on canonical field ordering.
    pub fn set(&mut self, name: &str, value: &str) {
        // Check if field already exists and update it
        for field in &mut self.fields {
            if field.name == name {
                field.value = value.to_string();
                return;
            }
        }

        // Field doesn't exist, insert at appropriate location
        // Try to detect if this is a source or binary package paragraph
        let field_order = if self.fields.iter().any(|f| f.name == "Source") {
            SOURCE_FIELD_ORDER
        } else if self.fields.iter().any(|f| f.name == "Package") {
            BINARY_FIELD_ORDER
        } else {
            // Default based on what we're inserting
            if name == "Source" {
                SOURCE_FIELD_ORDER
            } else if name == "Package" {
                BINARY_FIELD_ORDER
            } else {
                // Try to determine based on existing fields
                let has_source_fields = self.fields.iter().any(|f| {
                    SOURCE_FIELD_ORDER.contains(&f.name.as_str())
                        && !BINARY_FIELD_ORDER.contains(&f.name.as_str())
                });
                if has_source_fields {
                    SOURCE_FIELD_ORDER
                } else {
                    BINARY_FIELD_ORDER
                }
            }
        };

        let insertion_index = self.find_insertion_index(name, field_order);
        self.fields.insert(
            insertion_index,
            Field {
                name: name.to_string(),
                value: value.to_string(),
            },
        );
    }

    /// Set a field using a specific field ordering.
    pub fn set_with_field_order(&mut self, name: &str, value: &str, field_order: &[&str]) {
        // Check if field already exists and update it
        for field in &mut self.fields {
            if field.name == name {
                field.value = value.to_string();
                return;
            }
        }

        let insertion_index = self.find_insertion_index(name, field_order);
        self.fields.insert(
            insertion_index,
            Field {
                name: name.to_string(),
                value: value.to_string(),
            },
        );
    }

    /// Find the appropriate insertion index for a new field based on field ordering.
    fn find_insertion_index(&self, name: &str, field_order: &[&str]) -> usize {
        // Find position of the new field in the canonical order
        let new_field_position = field_order.iter().position(|&field| field == name);

        let mut insertion_index = self.fields.len();

        // Find the right position based on canonical field order
        for (i, field) in self.fields.iter().enumerate() {
            let existing_position = field_order.iter().position(|&f| f == field.name);

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
                    if name < &field.name {
                        insertion_index = i;
                        break;
                    }
                }
            }
        }

        // If we have a position in canonical order but haven't found where to insert yet,
        // we need to insert after all known fields that come before it
        if new_field_position.is_some() && insertion_index == self.fields.len() {
            // Look for the position after the last known field that comes before our field
            for (i, field) in self.fields.iter().enumerate().rev() {
                if field_order.iter().position(|&f| f == field.name).is_some() {
                    // Found a known field, insert after it
                    insertion_index = i + 1;
                    break;
                }
            }
        }

        insertion_index
    }

    /// Remove a field from the paragraph.
    pub fn remove(&mut self, name: &str) {
        self.fields.retain(|field| field.name != name);
    }
}

impl std::fmt::Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let lines = self.value.lines().collect::<Vec<_>>();
        if lines.len() > 1 {
            write!(f, "{}:", self.name)?;
            for line in lines {
                writeln!(f, " {}", line)?;
            }
            Ok(())
        } else {
            writeln!(f, "{}: {}", self.name, self.value)
        }
    }
}

impl std::fmt::Display for Paragraph {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for field in &self.fields {
            field.fmt(f)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Deb822 {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for (i, paragraph) in self.0.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            write!(f, "{}", paragraph)?;
        }
        Ok(())
    }
}

impl std::str::FromStr for Paragraph {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let doc: Deb822 = s.parse().map_err(|_| Error::ExpectedEof)?;
        if doc.is_empty() {
            Err(Error::UnexpectedEof)
        } else if doc.len() > 1 {
            Err(Error::ExpectedEof)
        } else {
            Ok(doc.0.into_iter().next().unwrap())
        }
    }
}

impl From<Vec<(String, String)>> for Paragraph {
    fn from(fields: Vec<(String, String)>) -> Self {
        fields.into_iter().collect()
    }
}

impl FromIterator<(String, String)> for Paragraph {
    fn from_iter<T: IntoIterator<Item = (String, String)>>(iter: T) -> Self {
        let fields = iter
            .into_iter()
            .map(|(name, value)| Field { name, value })
            .collect();
        Paragraph { fields }
    }
}

impl IntoIterator for Paragraph {
    type Item = (String, String);
    type IntoIter = std::iter::Map<std::vec::IntoIter<Field>, fn(Field) -> (String, String)>;

    fn into_iter(self) -> Self::IntoIter {
        self.fields
            .into_iter()
            .map(|field| (field.name, field.value))
    }
}

/// A deb822 document.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Deb822(Vec<Paragraph>);

impl From<Deb822> for Vec<Paragraph> {
    fn from(doc: Deb822) -> Self {
        doc.0
    }
}

impl IntoIterator for Deb822 {
    type Item = Paragraph;
    type IntoIter = std::vec::IntoIter<Paragraph>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Deb822 {
    /// Number of paragraphs in the document.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Check if the document is empty.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Iterate over the paragraphs in the document.
    pub fn iter(&self) -> impl Iterator<Item = &Paragraph> {
        self.0.iter()
    }

    /// Iterate over the paragraphs in the document, mutably.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Paragraph> {
        self.0.iter_mut()
    }

    /// Read from a reader.
    pub fn from_reader<R: std::io::Read>(mut r: R) -> Result<Self, Error> {
        let mut buf = String::new();
        r.read_to_string(&mut buf)?;
        buf.parse()
    }

    /// Stream paragraphs from a reader.
    ///
    /// This returns an iterator that reads and parses paragraphs one at a time,
    /// which is more memory-efficient for large files.
    pub fn iter_paragraphs_from_reader<R: std::io::BufRead>(reader: R) -> ParagraphReader<R> {
        ParagraphReader::new(reader)
    }
}

/// Reader that streams paragraphs from a buffered reader.
pub struct ParagraphReader<R: std::io::BufRead> {
    reader: R,
    buffer: String,
    finished: bool,
}

impl<R: std::io::BufRead> ParagraphReader<R> {
    /// Create a new paragraph reader from a buffered reader.
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            buffer: String::new(),
            finished: false,
        }
    }
}

impl<R: std::io::BufRead> Iterator for ParagraphReader<R> {
    type Item = Result<Paragraph, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        self.buffer.clear();
        let mut found_content = false;

        loop {
            let mut line = String::new();
            match self.reader.read_line(&mut line) {
                Ok(0) => {
                    // End of file
                    self.finished = true;
                    if found_content {
                        // Parse the buffered paragraph
                        return Some(self.buffer.parse());
                    }
                    return None;
                }
                Ok(_) => {
                    // Check if this is a blank line (paragraph separator)
                    if line.trim().is_empty() && found_content {
                        // End of current paragraph
                        return Some(self.buffer.parse());
                    }

                    // Skip leading blank lines and comments before first field
                    if !found_content
                        && (line.trim().is_empty() || line.trim_start().starts_with('#'))
                    {
                        continue;
                    }

                    // Check if this starts a new field (not indented)
                    if !line.starts_with(|c: char| c.is_whitespace()) && line.contains(':') {
                        found_content = true;
                    } else if found_content {
                        // Continuation line or comment within paragraph
                    } else if !line.trim_start().starts_with('#') {
                        // Non-blank, non-comment line before any field - this is content
                        found_content = true;
                    }

                    self.buffer.push_str(&line);
                }
                Err(e) => {
                    self.finished = true;
                    return Some(Err(Error::Io(e)));
                }
            }
        }
    }
}

impl std::str::FromStr for Deb822 {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut tokens = crate::lex::lex(s).peekable();

        let mut paragraphs = Vec::new();
        let mut current_paragraph = Vec::new();

        while let Some((k, t)) = tokens.next() {
            match k {
                SyntaxKind::INDENT | SyntaxKind::COLON | SyntaxKind::ERROR => {
                    return Err(Error::UnexpectedToken(k, t.to_string()));
                }
                SyntaxKind::WHITESPACE => {
                    // ignore whitespace
                }
                SyntaxKind::KEY => {
                    current_paragraph.push(Field {
                        name: t.to_string(),
                        value: String::new(),
                    });

                    match tokens.next() {
                        Some((SyntaxKind::COLON, _)) => {}
                        Some((k, t)) => {
                            return Err(Error::UnexpectedToken(k, t.to_string()));
                        }
                        None => {
                            return Err(Error::UnexpectedEof);
                        }
                    }

                    while tokens.peek().map(|(k, _)| k) == Some(&SyntaxKind::WHITESPACE) {
                        tokens.next();
                    }

                    for (k, t) in tokens.by_ref() {
                        match k {
                            SyntaxKind::VALUE => {
                                current_paragraph.last_mut().unwrap().value = t.to_string();
                            }
                            SyntaxKind::NEWLINE => {
                                break;
                            }
                            _ => return Err(Error::UnexpectedToken(k, t.to_string())),
                        }
                    }

                    current_paragraph.last_mut().unwrap().value.push('\n');

                    // while the next line starts with INDENT, it's a continuation of the value
                    while tokens.peek().map(|(k, _)| k) == Some(&SyntaxKind::INDENT) {
                        tokens.next();
                        loop {
                            match tokens.peek() {
                                Some((SyntaxKind::VALUE, t)) => {
                                    current_paragraph.last_mut().unwrap().value.push_str(t);
                                    tokens.next();
                                }
                                Some((SyntaxKind::COMMENT, _)) => {
                                    // ignore comments
                                    tokens.next();
                                }
                                Some((SyntaxKind::NEWLINE, n)) => {
                                    current_paragraph.last_mut().unwrap().value.push_str(n);
                                    tokens.next();
                                    break;
                                }
                                Some((SyntaxKind::KEY, _)) => {
                                    break;
                                }
                                Some((k, _)) => {
                                    return Err(Error::UnexpectedToken(*k, t.to_string()));
                                }
                                None => {
                                    break;
                                }
                            }
                        }
                    }

                    // Trim the trailing newline
                    {
                        let par = current_paragraph.last_mut().unwrap();
                        if par.value.ends_with('\n') {
                            par.value.pop();
                        }
                    }
                }
                SyntaxKind::VALUE => {
                    return Err(Error::UnexpectedToken(k, t.to_string()));
                }
                SyntaxKind::COMMENT => {
                    for (k, _) in tokens.by_ref() {
                        if k == SyntaxKind::NEWLINE {
                            break;
                        }
                    }
                }
                SyntaxKind::NEWLINE => {
                    if !current_paragraph.is_empty() {
                        paragraphs.push(Paragraph {
                            fields: current_paragraph,
                        });
                        current_paragraph = Vec::new();
                    }
                }
            }
        }
        if !current_paragraph.is_empty() {
            paragraphs.push(Paragraph {
                fields: current_paragraph,
            });
        }
        Ok(Deb822(paragraphs))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::lex;

    #[test]
    fn test_error_display() {
        let err = Error::UnexpectedToken(SyntaxKind::ERROR, "invalid".to_string());
        assert_eq!(err.to_string(), "Unexpected token: invalid");

        let err = Error::UnexpectedEof;
        assert_eq!(err.to_string(), "Unexpected end-of-file");

        let err = Error::ExpectedEof;
        assert_eq!(err.to_string(), "Expected end-of-file");

        let io_err = std::io::Error::other("test error");
        let err = Error::Io(io_err);
        assert!(err.to_string().contains("IO error: test error"));
    }

    #[test]
    fn test_parse() {
        let input = r#"Package: hello
Version: 2.10
Description: A program that says hello
 Some more text

Package: world
Version: 1.0
Description: A program that says world
 And some more text
Another-Field: value

# A comment

"#;

        let mut deb822: Deb822 = input.parse().unwrap();
        assert_eq!(
            deb822,
            Deb822(vec![
                Paragraph {
                    fields: vec![
                        Field {
                            name: "Package".to_string(),
                            value: "hello".to_string(),
                        },
                        Field {
                            name: "Version".to_string(),
                            value: "2.10".to_string(),
                        },
                        Field {
                            name: "Description".to_string(),
                            value: "A program that says hello\nSome more text".to_string(),
                        },
                    ],
                },
                Paragraph {
                    fields: vec![
                        Field {
                            name: "Package".to_string(),
                            value: "world".to_string(),
                        },
                        Field {
                            name: "Version".to_string(),
                            value: "1.0".to_string(),
                        },
                        Field {
                            name: "Description".to_string(),
                            value: "A program that says world\nAnd some more text".to_string(),
                        },
                        Field {
                            name: "Another-Field".to_string(),
                            value: "value".to_string(),
                        },
                    ],
                },
            ])
        );
        assert_eq!(deb822.len(), 2);
        assert!(!deb822.is_empty());
        assert_eq!(deb822.iter().count(), 2);

        let para = deb822.iter().next().unwrap();
        assert_eq!(para.get("Package"), Some("hello"));
        assert_eq!(para.get("Version"), Some("2.10"));
        assert_eq!(
            para.get("Description"),
            Some("A program that says hello\nSome more text")
        );
        assert_eq!(para.get("Another-Field"), None);
        assert!(!para.is_empty());
        assert_eq!(para.len(), 3);
        assert_eq!(
            para.iter().collect::<Vec<_>>(),
            vec![
                ("Package", "hello"),
                ("Version", "2.10"),
                ("Description", "A program that says hello\nSome more text"),
            ]
        );
        let para = deb822.iter_mut().next().unwrap();
        para.insert("Another-Field", "value");
        assert_eq!(para.get("Another-Field"), Some("value"));

        let mut newpara = Paragraph { fields: vec![] };
        newpara.insert("Package", "new");
        assert_eq!(newpara.to_string(), "Package: new\n");
    }

    #[test]
    fn test_lex() {
        let input = r#"Package: hello
Version: 2.10

Package: world
# Comment
Version: 1.0
Description: A program that says world
 And some more text
"#;
        assert_eq!(
            lex(input).collect::<Vec<_>>(),
            vec![
                (SyntaxKind::KEY, "Package"),
                (SyntaxKind::COLON, ":"),
                (SyntaxKind::WHITESPACE, " "),
                (SyntaxKind::VALUE, "hello"),
                (SyntaxKind::NEWLINE, "\n"),
                (SyntaxKind::KEY, "Version"),
                (SyntaxKind::COLON, ":"),
                (SyntaxKind::WHITESPACE, " "),
                (SyntaxKind::VALUE, "2.10"),
                (SyntaxKind::NEWLINE, "\n"),
                (SyntaxKind::NEWLINE, "\n"),
                (SyntaxKind::KEY, "Package"),
                (SyntaxKind::COLON, ":"),
                (SyntaxKind::WHITESPACE, " "),
                (SyntaxKind::VALUE, "world"),
                (SyntaxKind::NEWLINE, "\n"),
                (SyntaxKind::COMMENT, "# Comment"),
                (SyntaxKind::NEWLINE, "\n"),
                (SyntaxKind::KEY, "Version"),
                (SyntaxKind::COLON, ":"),
                (SyntaxKind::WHITESPACE, " "),
                (SyntaxKind::VALUE, "1.0"),
                (SyntaxKind::NEWLINE, "\n"),
                (SyntaxKind::KEY, "Description"),
                (SyntaxKind::COLON, ":"),
                (SyntaxKind::WHITESPACE, " "),
                (SyntaxKind::VALUE, "A program that says world"),
                (SyntaxKind::NEWLINE, "\n"),
                (SyntaxKind::INDENT, " "),
                (SyntaxKind::VALUE, "And some more text"),
                (SyntaxKind::NEWLINE, "\n"),
            ]
        );
    }

    #[test]
    fn test_paragraph_iter() {
        let input = r#"Package: hello
Version: 2.10
"#;
        let para: Paragraph = input.parse().unwrap();
        let mut iter = para.into_iter();
        assert_eq!(
            iter.next(),
            Some(("Package".to_string(), "hello".to_string()))
        );
        assert_eq!(
            iter.next(),
            Some(("Version".to_string(), "2.10".to_string()))
        );
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_format_multiline() {
        let para = Paragraph {
            fields: vec![Field {
                name: "Description".to_string(),
                value: "A program that says hello\nSome more text".to_string(),
            }],
        };

        assert_eq!(
            para.to_string(),
            "Description: A program that says hello\n Some more text\n"
        );
    }

    #[test]
    fn test_paragraph_from_str_errors() {
        // Test ExpectedEof error
        let result = "Package: foo\n\nPackage: bar\n".parse::<Paragraph>();
        assert!(matches!(result, Err(Error::ExpectedEof)));

        // Test UnexpectedEof error
        let result = "".parse::<Paragraph>();
        assert!(matches!(result, Err(Error::UnexpectedEof)));
    }

    #[test]
    fn test_from_vec() {
        let fields = vec![
            ("Package".to_string(), "hello".to_string()),
            ("Version".to_string(), "1.0".to_string()),
        ];

        let para: Paragraph = fields.into();
        assert_eq!(para.get("Package"), Some("hello"));
        assert_eq!(para.get("Version"), Some("1.0"));
    }

    #[test]
    fn test_unexpected_tokens() {
        // Test parsing with unexpected tokens
        let input = "Value before key\nPackage: hello\n";
        let result = input.parse::<Deb822>();
        assert!(matches!(result, Err(Error::UnexpectedToken(_, _))));

        // Test parsing with missing colon after key
        let input = "Package hello\n";
        let result = input.parse::<Deb822>();
        assert!(matches!(result, Err(Error::UnexpectedToken(_, _))));

        // Test parsing with unexpected indent
        let input = " Indented: value\n";
        let result = input.parse::<Deb822>();
        assert!(matches!(result, Err(Error::UnexpectedToken(_, _))));

        // Test parsing with unexpected value
        let input = "Key: value\nvalue without key\n";
        let result = input.parse::<Deb822>();
        assert!(matches!(result, Err(Error::UnexpectedToken(_, _))));

        // Test parsing with unexpected colon
        let input = "Key: value\n:\n";
        let result = input.parse::<Deb822>();
        assert!(matches!(result, Err(Error::UnexpectedToken(_, _))));
    }

    #[test]
    fn test_parse_continuation_with_colon() {
        // Test that continuation lines with colons are properly parsed
        let input = "Package: test\nDescription: short\n line: with colon\n";
        let result = input.parse::<Deb822>();
        assert!(result.is_ok());

        let deb822 = result.unwrap();
        assert_eq!(deb822.0.len(), 1);
        assert_eq!(deb822.0[0].fields.len(), 2);
        assert_eq!(deb822.0[0].fields[0].name, "Package");
        assert_eq!(deb822.0[0].fields[0].value, "test");
        assert_eq!(deb822.0[0].fields[1].name, "Description");
        assert_eq!(deb822.0[0].fields[1].value, "short\nline: with colon");
    }

    #[test]
    fn test_parse_continuation_starting_with_colon() {
        // Test continuation line STARTING with a colon (issue #315)
        let input = "Package: test\nDescription: short\n :value\n";
        let result = input.parse::<Deb822>();
        assert!(result.is_ok());

        let deb822 = result.unwrap();
        assert_eq!(deb822.0.len(), 1);
        assert_eq!(deb822.0[0].fields.len(), 2);
        assert_eq!(deb822.0[0].fields[0].name, "Package");
        assert_eq!(deb822.0[0].fields[0].value, "test");
        assert_eq!(deb822.0[0].fields[1].name, "Description");
        assert_eq!(deb822.0[0].fields[1].value, "short\n:value");
    }

    #[test]
    fn test_from_reader() {
        // Test Deb822::from_reader with valid input
        let input = "Package: hello\nVersion: 1.0\n";
        let result = Deb822::from_reader(input.as_bytes()).unwrap();
        assert_eq!(result.len(), 1);
        let para = result.iter().next().unwrap();
        assert_eq!(para.get("Package"), Some("hello"));

        // Test with IO error
        use std::io::Error as IoError;
        struct FailingReader;
        impl std::io::Read for FailingReader {
            fn read(&mut self, _: &mut [u8]) -> std::io::Result<usize> {
                Err(IoError::other("test error"))
            }
        }

        let result = Deb822::from_reader(FailingReader);
        assert!(matches!(result, Err(Error::Io(_))));
    }

    #[test]
    fn test_deb822_vec_conversion() {
        let paragraphs = vec![
            Paragraph {
                fields: vec![Field {
                    name: "Package".to_string(),
                    value: "hello".to_string(),
                }],
            },
            Paragraph {
                fields: vec![Field {
                    name: "Package".to_string(),
                    value: "world".to_string(),
                }],
            },
        ];

        let deb822 = Deb822(paragraphs.clone());
        let vec: Vec<Paragraph> = deb822.into();
        assert_eq!(vec, paragraphs);
    }

    #[test]
    fn test_deb822_iteration() {
        let paragraphs = vec![
            Paragraph {
                fields: vec![Field {
                    name: "Package".to_string(),
                    value: "hello".to_string(),
                }],
            },
            Paragraph {
                fields: vec![Field {
                    name: "Package".to_string(),
                    value: "world".to_string(),
                }],
            },
        ];

        let deb822 = Deb822(paragraphs.clone());

        // Test IntoIterator implementation
        let collected: Vec<_> = deb822.into_iter().collect();
        assert_eq!(collected, paragraphs);

        // Test iter() and iter_mut()
        let deb822 = Deb822(paragraphs.clone());
        let iter_refs: Vec<&Paragraph> = deb822.iter().collect();
        assert_eq!(iter_refs.len(), 2);
        assert_eq!(iter_refs[0].get("Package"), Some("hello"));

        let mut deb822 = Deb822(paragraphs.clone());
        for para in deb822.iter_mut() {
            if para.get("Package") == Some("hello") {
                para.set("Version", "1.0");
            }
        }
        assert_eq!(deb822.iter().next().unwrap().get("Version"), Some("1.0"));
    }

    #[test]
    fn test_empty_collections() {
        // Test empty Deb822
        let deb822 = Deb822(vec![]);
        assert!(deb822.is_empty());
        assert_eq!(deb822.len(), 0);
        assert_eq!(deb822.iter().count(), 0);

        // Test empty Paragraph
        let para = Paragraph { fields: vec![] };
        assert!(para.is_empty());
        assert_eq!(para.len(), 0);
        assert_eq!(para.iter().count(), 0);
        assert_eq!(para.get("Any"), None);

        // Test formatting of empty paragraph
        assert_eq!(para.to_string(), "");

        // Test formatting of empty Deb822
        assert_eq!(deb822.to_string(), "");
    }

    #[test]
    fn test_paragraph_mutable_iteration() {
        let mut para = Paragraph {
            fields: vec![
                Field {
                    name: "First".to_string(),
                    value: "1".to_string(),
                },
                Field {
                    name: "Second".to_string(),
                    value: "2".to_string(),
                },
            ],
        };

        // Test iter_mut
        for (_, value) in para.iter_mut() {
            *value = format!("{}0", value);
        }

        assert_eq!(para.get("First"), Some("10"));
        assert_eq!(para.get("Second"), Some("20"));
    }

    #[test]
    fn test_insert_duplicate_key() {
        let mut para = Paragraph {
            fields: vec![Field {
                name: "Key".to_string(),
                value: "Value1".to_string(),
            }],
        };

        // Insert will add a new field, even if the key already exists
        para.insert("Key", "Value2");

        assert_eq!(para.fields.len(), 2);
        assert_eq!(para.fields[0].value, "Value1");
        assert_eq!(para.fields[1].value, "Value2");

        // But get() will return the first occurrence
        assert_eq!(para.get("Key"), Some("Value1"));
    }

    #[test]
    fn test_multiline_field_format() {
        // Test display formatting for multiline field values
        let field = Field {
            name: "MultiField".to_string(),
            value: "line1\nline2\nline3".to_string(),
        };

        let formatted = format!("{}", field);
        assert_eq!(formatted, "MultiField: line1\n line2\n line3\n");

        // Test formatting within paragraph context
        let para = Paragraph {
            fields: vec![field],
        };

        let formatted = format!("{}", para);
        assert_eq!(formatted, "MultiField: line1\n line2\n line3\n");
    }

    #[test]
    fn test_paragraph_parsing_edge_cases() {
        // Test parsing empty value
        let input = "Key:\n";
        let para: Paragraph = input.parse().unwrap();
        assert_eq!(para.get("Key"), Some(""));

        // Test parsing value with just whitespace
        // Note: whitespace after the colon appears to be trimmed by the parser
        let input = "Key:    \n";
        let para: Paragraph = input.parse().unwrap();
        assert_eq!(para.get("Key"), Some(""));

        // Test parsing multiple empty lines between paragraphs
        let input = "Key1: value1\n\n\n\nKey2: value2\n";
        let deb822: Deb822 = input.parse().unwrap();
        assert_eq!(deb822.len(), 2);

        // Test parsing complex indentation
        // The parser preserves the indentation from the original file
        let input = "Key: value\n with\n  indentation\n   levels\n";
        let para: Paragraph = input.parse().unwrap();
        assert_eq!(para.get("Key"), Some("value\nwith\nindentation\nlevels"));
    }

    #[test]
    fn test_parse_complex() {
        // Test various edge cases in the parser
        let input = "# Comment at start\nKey1: val1\nKey2: \n indented\nKey3: val3\n\n# Comment between paragraphs\n\nKey4: val4\n";
        let deb822: Deb822 = input.parse().unwrap();

        assert_eq!(deb822.len(), 2);
        let paragraphs: Vec<Paragraph> = deb822.into();

        assert_eq!(paragraphs[0].get("Key2"), Some("\nindented"));
        assert_eq!(paragraphs[1].get("Key4"), Some("val4"));

        // Test parsing with an indented line immediately after a key
        let input = "Key:\n indented value\n";
        let para: Paragraph = input.parse().unwrap();
        assert_eq!(para.get("Key"), Some("\nindented value"));
    }

    #[test]
    fn test_deb822_display() {
        // Test the Deb822::fmt Display implementation (lines 158-164)
        let para1 = Paragraph {
            fields: vec![Field {
                name: "Key1".to_string(),
                value: "Value1".to_string(),
            }],
        };

        let para2 = Paragraph {
            fields: vec![Field {
                name: "Key2".to_string(),
                value: "Value2".to_string(),
            }],
        };

        let deb822 = Deb822(vec![para1, para2]);
        let formatted = format!("{}", deb822);

        assert_eq!(formatted, "Key1: Value1\n\nKey2: Value2\n");
    }

    #[test]
    fn test_parser_edge_cases() {
        // Let's focus on testing various parser behaviors rather than expecting errors

        // Test comment handling
        let input = "# Comment\nKey: value";
        let deb822: Deb822 = input.parse().unwrap();
        assert_eq!(deb822.len(), 1);

        // Test for unexpected token at line 303
        let input = "Key: value\n .indented";
        let deb822: Deb822 = input.parse().unwrap();
        assert_eq!(
            deb822.iter().next().unwrap().get("Key"),
            Some("value\n.indented")
        );

        // Test multi-line values
        let input = "Key: value\n line1\n line2\n\nNextKey: value";
        let deb822: Deb822 = input.parse().unwrap();
        assert_eq!(deb822.len(), 2);
        assert_eq!(
            deb822.iter().next().unwrap().get("Key"),
            Some("value\nline1\nline2")
        );
    }

    #[test]
    fn test_iter_paragraphs_from_reader() {
        use std::io::BufReader;

        let input = r#"Package: hello
Version: 2.10
Description: A program that says hello
 Some more text

Package: world
Version: 1.0
Description: A program that says world
 And some more text
Another-Field: value

# A comment

"#;

        let reader = BufReader::new(input.as_bytes());
        let paragraphs: Result<Vec<_>, _> = Deb822::iter_paragraphs_from_reader(reader).collect();
        let paragraphs = paragraphs.unwrap();

        assert_eq!(paragraphs.len(), 2);

        assert_eq!(paragraphs[0].get("Package"), Some("hello"));
        assert_eq!(paragraphs[0].get("Version"), Some("2.10"));
        assert_eq!(
            paragraphs[0].get("Description"),
            Some("A program that says hello\nSome more text")
        );

        assert_eq!(paragraphs[1].get("Package"), Some("world"));
        assert_eq!(paragraphs[1].get("Version"), Some("1.0"));
        assert_eq!(
            paragraphs[1].get("Description"),
            Some("A program that says world\nAnd some more text")
        );
        assert_eq!(paragraphs[1].get("Another-Field"), Some("value"));
    }

    #[test]
    fn test_iter_paragraphs_from_reader_empty() {
        use std::io::BufReader;

        let input = "";
        let reader = BufReader::new(input.as_bytes());
        let paragraphs: Result<Vec<_>, _> = Deb822::iter_paragraphs_from_reader(reader).collect();
        let paragraphs = paragraphs.unwrap();

        assert_eq!(paragraphs.len(), 0);
    }

    #[test]
    fn test_iter_paragraphs_from_reader_with_leading_comments() {
        use std::io::BufReader;

        let input = r#"# Leading comment
# Another comment

Package: test
Version: 1.0
"#;

        let reader = BufReader::new(input.as_bytes());
        let paragraphs: Result<Vec<_>, _> = Deb822::iter_paragraphs_from_reader(reader).collect();
        let paragraphs = paragraphs.unwrap();

        assert_eq!(paragraphs.len(), 1);
        assert_eq!(paragraphs[0].get("Package"), Some("test"));
    }
}
