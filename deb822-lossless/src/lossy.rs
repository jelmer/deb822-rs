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

    /// Set the value of a field.
    ///
    /// If a field with the same name already exists, its value
    /// will be updated.
    pub fn set(&mut self, name: &str, value: &str) {
        for field in &mut self.fields {
            if field.name == name {
                field.value.clear();
                field.value.push_str(value);
                return;
            }
        }
        self.insert(name, value);
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
                    assert_eq!(
                        current_paragraph.last_mut().unwrap().value.pop(),
                        Some('\n')
                    );
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
}
