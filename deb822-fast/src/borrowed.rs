//! Low-allocation borrowed API for deb822 parsing.
//!
//! This module provides a borrowed-data parser that avoids allocating owned Strings,
//! instead returning borrowed string slices from the source. This is significantly
//! faster than the owned API but requires lifetime management.
//!
//! ## Allocations
//!
//! While string data is borrowed (zero string allocations), the parser does allocate:
//! - `Vec<BorrowedParagraph>` to hold paragraphs
//! - `Vec<BorrowedField>` to hold fields within each paragraph
//! - `Vec<&str>` for multi-line field values (single-line fields avoid this)
//!
//! Despite these allocations, this is still much faster than the owned API since
//! it avoids copying all the field names and values into owned Strings.

use crate::Error;

/// Field value representation that avoids allocation for single-line fields.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FieldValue<'a> {
    /// Single-line field (no allocation)
    Single(&'a str),
    /// Multi-line field with continuation lines
    Multi(Vec<&'a str>),
}

/// A borrowed field that references data in the source string.
///
/// The name is always borrowed. The value is either a single line or multiple lines.
/// Single-line fields (the common case) avoid Vec allocation entirely.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BorrowedField<'a> {
    /// The field name (borrowed from source)
    pub name: &'a str,
    /// The field value (single or multiple lines)
    pub value: FieldValue<'a>,
}

impl<'a> BorrowedField<'a> {
    /// Get the value as a single line (for single-line fields).
    /// Returns None if the field has multiple lines.
    pub fn as_single_line(&self) -> Option<&'a str> {
        match &self.value {
            FieldValue::Single(s) => Some(s),
            FieldValue::Multi(_) => None,
        }
    }

    /// Get the value lines as a slice.
    pub fn lines(&self) -> &[&'a str] {
        match &self.value {
            FieldValue::Single(s) => std::slice::from_ref(s),
            FieldValue::Multi(v) => v.as_slice(),
        }
    }

    /// Join the value lines into an owned String with newlines.
    pub fn join(&self) -> String {
        match &self.value {
            FieldValue::Single(s) => s.to_string(),
            FieldValue::Multi(v) => v.join("\n"),
        }
    }

    /// Check if this is a single-line field.
    pub fn is_single_line(&self) -> bool {
        matches!(self.value, FieldValue::Single(_))
    }

    /// Check if this is a multi-line field.
    pub fn is_multi_line(&self) -> bool {
        matches!(self.value, FieldValue::Multi(_))
    }
}

/// A borrowed paragraph that references data in the source string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BorrowedParagraph<'a> {
    fields: Vec<BorrowedField<'a>>,
}

impl<'a> BorrowedParagraph<'a> {
    /// Get a field by name.
    pub fn get_field(&self, name: &str) -> Option<&BorrowedField<'a>> {
        self.fields.iter().find(|f| f.name == name)
    }

    /// Get a field value by name as lines.
    pub fn get(&self, name: &str) -> Option<&[&'a str]> {
        self.fields
            .iter()
            .find(|f| f.name == name)
            .map(|f| f.lines())
    }

    /// Get a single-line field value by name.
    /// Returns None if the field doesn't exist or has multiple lines.
    pub fn get_single(&self, name: &str) -> Option<&'a str> {
        self.fields
            .iter()
            .find(|f| f.name == name)
            .and_then(|f| f.as_single_line())
    }

    /// Iterate over all fields.
    pub fn iter(&self) -> impl Iterator<Item = &BorrowedField<'a>> + '_ {
        self.fields.iter()
    }

    /// Number of fields in the paragraph.
    pub fn len(&self) -> usize {
        self.fields.len()
    }

    /// Check if the paragraph is empty.
    pub fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }
}

/// Low-allocation parser that returns borrowed paragraphs.
///
/// This parser borrows all string data from the input, avoiding String allocations.
/// It does allocate Vec structures to hold the paragraph and field lists.
pub struct BorrowedParser<'a> {
    input: &'a str,
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> BorrowedParser<'a> {
    /// Create a new borrowed parser from a string slice.
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            bytes: input.as_bytes(),
            pos: 0,
        }
    }

    /// Parse all paragraphs and return them as a Vec.
    ///
    /// Note: This still allocates the Vec and field lists, but the strings
    /// themselves are borrowed.
    pub fn parse_all(mut self) -> Result<Vec<BorrowedParagraph<'a>>, Error> {
        let mut paragraphs = Vec::with_capacity(16);

        while let Some(para) = self.next_paragraph()? {
            paragraphs.push(para);
        }

        Ok(paragraphs)
    }

    /// Parse the next paragraph, returning None if at end.
    fn next_paragraph(&mut self) -> Result<Option<BorrowedParagraph<'a>>, Error> {
        let len = self.bytes.len();

        // Skip leading whitespace and comments between paragraphs
        loop {
            if self.pos >= len {
                return Ok(None);
            }

            let b = self.bytes[self.pos];
            if b == b'#' {
                // Skip comment line
                while self.pos < len && self.bytes[self.pos] != b'\n' {
                    self.pos += 1;
                }
                if self.pos < len {
                    self.pos += 1;
                }
            } else if b == b'\n' || b == b'\r' {
                // Skip newline
                self.pos += 1;
            } else if b == b' ' || b == b'\t' {
                // Skip whitespace-only line (but check if it contains actual content)
                let line_start = self.pos;
                while self.pos < len
                    && (self.bytes[self.pos] == b' ' || self.bytes[self.pos] == b'\t')
                {
                    self.pos += 1;
                }
                if self.pos < len && self.bytes[self.pos] != b'\n' && self.bytes[self.pos] != b'\r'
                {
                    // There's non-whitespace content after spaces - this is an error
                    while self.pos < len && self.bytes[self.pos] != b'\n' {
                        self.pos += 1;
                    }
                    let token = &self.input[line_start..self.pos];
                    return Err(Error::UnexpectedToken(token.to_string()));
                }
                // Just whitespace on the line, skip past it
                if self.pos < len {
                    self.pos += 1; // Skip newline
                }
            } else {
                // Found start of content
                break;
            }
        }

        let mut fields: Vec<BorrowedField<'a>> = Vec::with_capacity(8);

        loop {
            if self.pos >= len {
                break;
            }

            // Check for blank line (end of paragraph)
            if self.bytes[self.pos] == b'\n' {
                self.pos += 1;
                break;
            }

            // Skip comment lines
            if self.bytes[self.pos] == b'#' {
                while self.pos < len && self.bytes[self.pos] != b'\n' {
                    self.pos += 1;
                }
                if self.pos < len {
                    self.pos += 1;
                }
                continue;
            }

            // Check for continuation line
            if self.bytes[self.pos] == b' ' || self.bytes[self.pos] == b'\t' {
                if fields.is_empty() {
                    // Indented line before any field - this is an error
                    let line_start = self.pos;
                    while self.pos < len && self.bytes[self.pos] != b'\n' {
                        self.pos += 1;
                    }
                    let token = &self.input[line_start..self.pos];
                    return Err(Error::UnexpectedToken(token.to_string()));
                }

                // This is a continuation line - append to the last field's value vec
                // Skip all leading whitespace (deb822 format strips leading spaces)
                while self.pos < len
                    && (self.bytes[self.pos] == b' ' || self.bytes[self.pos] == b'\t')
                {
                    self.pos += 1;
                }

                // Read the continuation line
                let line_start = self.pos;
                while self.pos < len && self.bytes[self.pos] != b'\n' {
                    self.pos += 1;
                }

                if let Some(last_field) = fields.last_mut() {
                    // Add the continuation line - convert Single to Multi if needed
                    match &mut last_field.value {
                        FieldValue::Single(first) => {
                            // Convert to Multi with two lines
                            let first = *first;
                            last_field.value =
                                FieldValue::Multi(vec![first, &self.input[line_start..self.pos]]);
                        }
                        FieldValue::Multi(lines) => {
                            lines.push(&self.input[line_start..self.pos]);
                        }
                    }
                }

                if self.pos < len {
                    self.pos += 1; // Skip newline
                }
                continue;
            }

            // Parse field name
            let name_start = self.pos;
            while self.pos < len && self.bytes[self.pos] != b':' && self.bytes[self.pos] != b'\n' {
                self.pos += 1;
            }

            if self.pos >= len || self.bytes[self.pos] != b':' {
                // Invalid line - return error
                let line_start = name_start;
                while self.pos < len && self.bytes[self.pos] != b'\n' {
                    self.pos += 1;
                }
                let token = &self.input[line_start..self.pos];
                return Err(Error::UnexpectedToken(token.to_string()));
            }

            let name = &self.input[name_start..self.pos];

            // Check for empty field name
            if name.is_empty() {
                let line_start = name_start;
                let mut end = self.pos;
                while end < len && self.bytes[end] != b'\n' {
                    end += 1;
                }
                let token = &self.input[line_start..end];
                return Err(Error::UnexpectedToken(token.to_string()));
            }

            self.pos += 1; // Skip colon

            // Skip whitespace after colon
            while self.pos < len && (self.bytes[self.pos] == b' ' || self.bytes[self.pos] == b'\t')
            {
                self.pos += 1;
            }

            // Parse field value (first line)
            let value_start = self.pos;
            while self.pos < len && self.bytes[self.pos] != b'\n' {
                self.pos += 1;
            }

            let value = FieldValue::Single(&self.input[value_start..self.pos]);

            fields.push(BorrowedField { name, value });

            if self.pos < len {
                self.pos += 1; // Skip newline
            }
        }

        if fields.is_empty() {
            Ok(None)
        } else {
            Ok(Some(BorrowedParagraph { fields }))
        }
    }
}

/// Iterator that yields borrowed paragraphs.
pub struct BorrowedParagraphIter<'a> {
    parser: BorrowedParser<'a>,
    done: bool,
}

impl<'a> BorrowedParagraphIter<'a> {
    /// Create a new iterator from input.
    pub fn new(input: &'a str) -> Self {
        Self {
            parser: BorrowedParser::new(input),
            done: false,
        }
    }
}

impl<'a> Iterator for BorrowedParagraphIter<'a> {
    type Item = Result<BorrowedParagraph<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }

        match self.parser.next_paragraph() {
            Ok(Some(para)) => Some(Ok(para)),
            Ok(None) => {
                self.done = true;
                None
            }
            Err(e) => {
                self.done = true;
                Some(Err(e))
            }
        }
    }
}

/// Parse borrowed paragraphs from input.
pub fn parse_borrowed(input: &str) -> Result<Vec<BorrowedParagraph<'_>>, Error> {
    BorrowedParser::new(input).parse_all()
}

/// Iterate over borrowed paragraphs.
pub fn iter_paragraphs_borrowed(input: &str) -> BorrowedParagraphIter<'_> {
    BorrowedParagraphIter::new(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_borrowed_parser_simple() {
        let input = "Package: hello\nVersion: 1.0\n\nPackage: world\nVersion: 2.0\n";
        let paragraphs = parse_borrowed(input).unwrap();

        assert_eq!(paragraphs.len(), 2);
        assert_eq!(paragraphs[0].get_single("Package"), Some("hello"));
        assert_eq!(paragraphs[0].get_single("Version"), Some("1.0"));
        assert_eq!(paragraphs[1].get_single("Package"), Some("world"));
        assert_eq!(paragraphs[1].get_single("Version"), Some("2.0"));
    }

    #[test]
    fn test_borrowed_iter() {
        let input = "Package: test\nVersion: 1.0\n";
        let mut iter = iter_paragraphs_borrowed(input);

        let para = iter.next().unwrap().unwrap();
        assert_eq!(para.get_single("Package"), Some("test"));
        assert!(iter.next().is_none());
    }

    #[test]
    fn test_borrowed_field_iter() {
        let input = "A: 1\nB: 2\nC: 3\n";
        let paragraphs = parse_borrowed(input).unwrap();

        let fields: Vec<_> = paragraphs[0].iter().collect();
        assert_eq!(fields.len(), 3);
        assert_eq!(fields[0].name, "A");
        assert_eq!(fields[0].value, FieldValue::Single("1"));
        assert_eq!(fields[0].as_single_line(), Some("1"));
    }

    #[test]
    fn test_borrowed_multiline_values() {
        let input = "Package: test\nDescription: short desc\n continuation line\n  another line\n";
        let paragraphs = parse_borrowed(input).unwrap();

        assert_eq!(paragraphs.len(), 1);
        assert_eq!(paragraphs[0].get_single("Package"), Some("test"));

        // For multiline fields, get() returns a slice of lines
        let desc_lines = paragraphs[0].get("Description").unwrap();
        assert_eq!(desc_lines.len(), 3);
        assert_eq!(desc_lines[0], "short desc");
        assert_eq!(desc_lines[1], "continuation line");
        assert_eq!(desc_lines[2], "another line");

        // Can also join to get the full value
        assert_eq!(
            paragraphs[0].get_field("Description").unwrap().join(),
            "short desc\ncontinuation line\nanother line"
        );
    }

    #[test]
    fn test_borrowed_with_comments() {
        let input = "# Comment at start\nPackage: hello\n# Mid comment\nVersion: 1.0\n";
        let paragraphs = parse_borrowed(input).unwrap();

        assert_eq!(paragraphs.len(), 1);
        assert_eq!(paragraphs[0].get_single("Package"), Some("hello"));
        assert_eq!(paragraphs[0].get_single("Version"), Some("1.0"));
    }

    #[test]
    fn test_borrowed_empty_value() {
        let input = "Package: test\nDescription:\n extra line\n";
        let paragraphs = parse_borrowed(input).unwrap();

        assert_eq!(paragraphs.len(), 1);
        let desc_lines = paragraphs[0].get("Description").unwrap();
        assert_eq!(desc_lines.len(), 2);
        assert_eq!(desc_lines[0], "");
        assert_eq!(desc_lines[1], "extra line");
    }

    #[test]
    fn test_borrowed_multiple_paragraphs() {
        let input = "A: 1\n\nB: 2\n\n\nC: 3\n";
        let paragraphs = parse_borrowed(input).unwrap();

        assert_eq!(paragraphs.len(), 3);
        assert_eq!(paragraphs[0].get_single("A"), Some("1"));
        assert_eq!(paragraphs[1].get_single("B"), Some("2"));
        assert_eq!(paragraphs[2].get_single("C"), Some("3"));
    }

    #[test]
    fn test_borrowed_error_unexpected_indent() {
        let input = " Indented: value\n";
        let result = parse_borrowed(input);
        assert!(matches!(result, Err(Error::UnexpectedToken(_))));
    }

    #[test]
    fn test_borrowed_error_missing_colon() {
        let input = "Package test\n";
        let result = parse_borrowed(input);
        assert!(matches!(result, Err(Error::UnexpectedToken(_))));
    }

    #[test]
    fn test_borrowed_error_empty_field_name() {
        let input = "Package: test\n:\n";
        let result = parse_borrowed(input);
        assert!(matches!(result, Err(Error::UnexpectedToken(_))));
    }

    #[test]
    fn test_borrowed_continuation_with_colon() {
        let input = "Package: test\nDescription: short\n line: with colon\n";
        let paragraphs = parse_borrowed(input).unwrap();

        assert_eq!(paragraphs.len(), 1);
        let desc_lines = paragraphs[0].get("Description").unwrap();
        assert_eq!(desc_lines.len(), 2);
        assert_eq!(desc_lines[0], "short");
        assert_eq!(desc_lines[1], "line: with colon");
    }

    #[test]
    fn test_borrowed_paragraph_len() {
        let input = "A: 1\nB: 2\nC: 3\n";
        let paragraphs = parse_borrowed(input).unwrap();

        assert_eq!(paragraphs[0].len(), 3);
        assert!(!paragraphs[0].is_empty());
    }

    #[test]
    fn test_borrowed_iter_paragraphs() {
        let input = "A: 1\n\nB: 2\n\nC: 3\n";
        let result: Result<Vec<_>, _> = iter_paragraphs_borrowed(input).collect();
        let paragraphs = result.unwrap();

        assert_eq!(paragraphs.len(), 3);
        assert_eq!(paragraphs[0].get_single("A"), Some("1"));
        assert_eq!(paragraphs[1].get_single("B"), Some("2"));
        assert_eq!(paragraphs[2].get_single("C"), Some("3"));
    }

    #[test]
    fn test_borrowed_empty_input() {
        let input = "";
        let paragraphs = parse_borrowed(input).unwrap();
        assert_eq!(paragraphs.len(), 0);
    }

    #[test]
    fn test_borrowed_only_whitespace() {
        let input = "\n\n  \n\t\n";
        let paragraphs = parse_borrowed(input).unwrap();
        assert_eq!(paragraphs.len(), 0);
    }

    #[test]
    fn test_borrowed_only_comments() {
        let input = "# Comment 1\n# Comment 2\n";
        let paragraphs = parse_borrowed(input).unwrap();
        assert_eq!(paragraphs.len(), 0);
    }

    #[test]
    fn test_borrowed_complex_debian_control() {
        let input = r#"Source: test-package
Section: utils
Priority: optional
Maintainer: Test User <test@example.com>
Build-Depends: debhelper (>= 10)
Standards-Version: 4.1.3

Package: test-package
Architecture: any
Depends: ${shlibs:Depends}, ${misc:Depends}
Description: A test package
 This is a longer description
 that spans multiple lines.
 .
 It even has a paragraph break.
"#;
        let paragraphs = parse_borrowed(input).unwrap();

        assert_eq!(paragraphs.len(), 2);

        // Source paragraph
        assert_eq!(paragraphs[0].get_single("Source"), Some("test-package"));
        assert_eq!(paragraphs[0].get_single("Section"), Some("utils"));
        assert_eq!(paragraphs[0].get_single("Priority"), Some("optional"));
        assert_eq!(
            paragraphs[0].get_single("Maintainer"),
            Some("Test User <test@example.com>")
        );

        // Binary paragraph
        assert_eq!(paragraphs[1].get_single("Package"), Some("test-package"));
        assert_eq!(paragraphs[1].get_single("Architecture"), Some("any"));

        // Description is multi-line
        let desc_lines = paragraphs[1].get("Description").unwrap();
        assert_eq!(desc_lines[0], "A test package");
        assert_eq!(desc_lines[1], "This is a longer description");
        assert_eq!(desc_lines[2], "that spans multiple lines.");
        assert_eq!(desc_lines[3], ".");
        assert_eq!(desc_lines[4], "It even has a paragraph break.");

        let full_desc = paragraphs[1].get_field("Description").unwrap().join();
        assert!(full_desc.contains("test package"));
        assert!(full_desc.contains("paragraph break"));
    }
}
