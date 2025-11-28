//! A library for parsing and manipulating debian/copyright files that
//! use the DEP-5 format.
//!
//! This library is intended to be used for manipulating debian/copyright
//!
//! # Examples
//!
//! ```rust
//!
//! use debian_copyright::lossless::Copyright;
//! use std::path::Path;
//!
//! let text = r#"Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/
//! Upstream-Author: John Doe <john@example>
//! Upstream-Name: example
//! Source: https://example.com/example
//!
//! Files: *
//! License: GPL-3+
//! Copyright: 2019 John Doe
//!
//! Files: debian/*
//! License: GPL-3+
//! Copyright: 2019 Jane Packager
//!
//! License: GPL-3+
//!  This program is free software: you can redistribute it and/or modify
//!  it under the terms of the GNU General Public License as published by
//!  the Free Software Foundation, either version 3 of the License, or
//!  (at your option) any later version.
//! "#;
//!
//! let c = text.parse::<Copyright>().unwrap();
//! let license = c.find_license_for_file(Path::new("debian/foo")).unwrap();
//! assert_eq!(license.name(), Some("GPL-3+"));
//! ```

use crate::{License, CURRENT_FORMAT, KNOWN_FORMATS};
use deb822_lossless::{Deb822, Paragraph};
use std::path::Path;

/// Field order for header paragraphs according to DEP-5 specification
const HEADER_FIELD_ORDER: &[&str] = &[
    "Format",
    "Upstream-Name",
    "Upstream-Contact",
    "Source",
    "Disclaimer",
    "Comment",
    "License",
    "Copyright",
];

/// Field order for Files paragraphs according to DEP-5 specification
const FILES_FIELD_ORDER: &[&str] = &["Files", "Copyright", "License", "Comment"];

/// Field order for standalone License paragraphs according to DEP-5 specification
const LICENSE_FIELD_ORDER: &[&str] = &["License", "Comment"];

/// A copyright file
#[derive(Debug)]
pub struct Copyright(Deb822);

impl Copyright {
    /// Create a new copyright file, with the current format
    pub fn new() -> Self {
        let mut deb822 = Deb822::new();
        let mut header = deb822.add_paragraph();
        header.set("Format", CURRENT_FORMAT);
        Copyright(deb822)
    }

    /// Create a new empty copyright file
    ///
    /// The difference with `new` is that this does not add the `Format` field.
    pub fn empty() -> Self {
        Self(Deb822::new())
    }

    /// Return the header paragraph
    pub fn header(&self) -> Option<Header> {
        self.0.paragraphs().next().map(Header)
    }

    /// Iterate over all files paragraphs
    pub fn iter_files(&self) -> impl Iterator<Item = FilesParagraph> {
        self.0
            .paragraphs()
            .filter(|x| x.contains_key("Files"))
            .map(FilesParagraph)
    }

    /// Iter over all license paragraphs
    pub fn iter_licenses(&self) -> impl Iterator<Item = LicenseParagraph> {
        self.0
            .paragraphs()
            .filter(|x| {
                !x.contains_key("Files") && !x.contains_key("Format") && x.contains_key("License")
            })
            .map(LicenseParagraph)
    }

    /// Returns the Files paragraph for the given filename.
    ///
    /// Consistent with the specification, this returns the last paragraph
    /// that matches (which should be the most specific)
    pub fn find_files(&self, filename: &Path) -> Option<FilesParagraph> {
        self.iter_files().filter(|p| p.matches(filename)).last()
    }

    /// Find license by name
    ///
    /// This will return the first license paragraph that has the given name.
    pub fn find_license_by_name(&self, name: &str) -> Option<License> {
        self.iter_licenses()
            .find(|p| p.name().as_deref() == Some(name))
            .map(|x| x.into())
    }

    /// Returns the license for the given file.
    pub fn find_license_for_file(&self, filename: &Path) -> Option<License> {
        let files = self.find_files(filename)?;
        let license = files.license()?;
        if license.text().is_some() {
            return Some(license);
        }
        self.find_license_by_name(license.name()?)
    }

    /// Read copyright file from a string, allowing syntax errors
    pub fn from_str_relaxed(s: &str) -> Result<(Self, Vec<String>), Error> {
        if !s.starts_with("Format:") {
            return Err(Error::NotMachineReadable);
        }

        let (deb822, errors) = Deb822::from_str_relaxed(s);
        Ok((Self(deb822), errors))
    }

    /// Read copyright file from a file, allowing syntax errors
    pub fn from_file_relaxed<P: AsRef<Path>>(path: P) -> Result<(Self, Vec<String>), Error> {
        let text = std::fs::read_to_string(path)?;
        Self::from_str_relaxed(&text)
    }

    /// Read copyright file from a file
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self, Error> {
        let text = std::fs::read_to_string(path)?;
        use std::str::FromStr;
        Self::from_str(&text)
    }

    /// Add a new files paragraph
    ///
    /// Returns a mutable reference to the newly created FilesParagraph
    pub fn add_files(
        &mut self,
        files: &[&str],
        copyright: &[&str],
        license: &License,
    ) -> FilesParagraph {
        let mut para = self.0.add_paragraph();
        para.set_with_field_order("Files", &files.join(" "), FILES_FIELD_ORDER);
        para.set_with_field_order("Copyright", &copyright.join("\n"), FILES_FIELD_ORDER);
        let license_text = match license {
            License::Name(name) => name.to_string(),
            License::Named(name, text) => format!("{}\n{}", name, text),
            License::Text(text) => text.to_string(),
        };
        para.set_with_field_order("License", &license_text, FILES_FIELD_ORDER);
        FilesParagraph(para)
    }

    /// Add a new license paragraph
    ///
    /// Returns a mutable reference to the newly created LicenseParagraph
    pub fn add_license(&mut self, license: &License) -> LicenseParagraph {
        let mut para = self.0.add_paragraph();
        let license_text = match license {
            License::Name(name) => name.to_string(),
            License::Named(name, text) => format!("{}\n{}", name, text),
            License::Text(text) => text.to_string(),
        };
        para.set_with_field_order("License", &license_text, LICENSE_FIELD_ORDER);
        LicenseParagraph(para)
    }

    /// Remove a license paragraph by its short name
    ///
    /// This removes the first standalone license paragraph that matches the given name.
    /// Returns true if a paragraph was removed, false otherwise.
    pub fn remove_license_by_name(&mut self, name: &str) -> bool {
        // Find the index of the license paragraph
        let mut index = None;
        for (i, para) in self.0.paragraphs().enumerate() {
            if !para.contains_key("Files")
                && !para.contains_key("Format")
                && para.contains_key("License")
            {
                let license_para = LicenseParagraph(para);
                if license_para.name().as_deref() == Some(name) {
                    index = Some(i);
                    break;
                }
            }
        }

        if let Some(i) = index {
            self.0.remove_paragraph(i);
            true
        } else {
            false
        }
    }

    /// Remove a files paragraph by matching file pattern
    ///
    /// This removes the first files paragraph where the Files field contains the given pattern.
    /// Returns true if a paragraph was removed, false otherwise.
    pub fn remove_files_by_pattern(&mut self, pattern: &str) -> bool {
        // Find the index of the files paragraph
        let mut index = None;
        for (i, para) in self.0.paragraphs().enumerate() {
            if para.contains_key("Files") {
                let files_para = FilesParagraph(para);
                if files_para.files().iter().any(|f| f == pattern) {
                    index = Some(i);
                    break;
                }
            }
        }

        if let Some(i) = index {
            self.0.remove_paragraph(i);
            true
        } else {
            false
        }
    }
}

/// Error parsing copyright files
#[derive(Debug)]
pub enum Error {
    /// Parse error
    ParseError(deb822_lossless::ParseError),

    /// IO error
    IoError(std::io::Error),

    /// The file is not machine readable
    NotMachineReadable,
}

impl From<deb822_lossless::Error> for Error {
    fn from(e: deb822_lossless::Error) -> Self {
        match e {
            deb822_lossless::Error::ParseError(e) => Error::ParseError(e),
            deb822_lossless::Error::IoError(e) => Error::IoError(e),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::IoError(e)
    }
}

impl From<deb822_lossless::ParseError> for Error {
    fn from(e: deb822_lossless::ParseError) -> Self {
        Error::ParseError(e)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self {
            Error::ParseError(e) => write!(f, "parse error: {}", e),
            Error::NotMachineReadable => write!(f, "not machine readable"),
            Error::IoError(e) => write!(f, "io error: {}", e),
        }
    }
}

impl std::error::Error for Error {}

impl Default for Copyright {
    fn default() -> Self {
        Copyright(Deb822::new())
    }
}

impl std::str::FromStr for Copyright {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !s.starts_with("Format:") {
            return Err(Error::NotMachineReadable);
        }
        Ok(Self(Deb822::from_str(s)?))
    }
}

impl std::fmt::Display for Copyright {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(&self.0.to_string())
    }
}

/// A header paragraph
pub struct Header(Paragraph);

impl Header {
    /// Returns the format string for this file.
    pub fn format_string(&self) -> Option<String> {
        self.0
            .get("Format")
            .or_else(|| self.0.get("Format-Specification"))
    }

    /// Return the underlying Deb822 paragraph
    pub fn as_deb822(&self) -> &Paragraph {
        &self.0
    }

    /// Return the underlying Deb822 paragraph, mutably
    pub fn as_mut_deb822(&mut self) -> &mut Paragraph {
        &mut self.0
    }

    /// Upstream name
    pub fn upstream_name(&self) -> Option<String> {
        self.0.get("Upstream-Name")
    }

    /// Set the upstream name
    pub fn set_upstream_name(&mut self, name: &str) {
        self.0
            .set_with_field_order("Upstream-Name", name, HEADER_FIELD_ORDER);
    }

    /// Upstream contact
    pub fn upstream_contact(&self) -> Option<String> {
        self.0.get("Upstream-Contact")
    }

    /// Set the upstream contact
    pub fn set_upstream_contact(&mut self, contact: &str) {
        self.0
            .set_with_field_order("Upstream-Contact", contact, HEADER_FIELD_ORDER);
    }

    /// Source
    pub fn source(&self) -> Option<String> {
        self.0.get("Source")
    }

    /// Set the source
    pub fn set_source(&mut self, source: &str) {
        self.0
            .set_with_field_order("Source", source, HEADER_FIELD_ORDER);
    }

    /// List of files excluded from the copyright information, as well as the source package
    pub fn files_excluded(&self) -> Option<Vec<String>> {
        self.0
            .get("Files-Excluded")
            .map(|x| x.split('\n').map(|x| x.to_string()).collect::<Vec<_>>())
    }

    /// Set excluded files
    pub fn set_files_excluded(&mut self, files: &[&str]) {
        self.0
            .set_with_field_order("Files-Excluded", &files.join("\n"), HEADER_FIELD_ORDER);
    }

    /// Fix the the header paragraph
    ///
    /// Currently this just renames `Format-Specification` to `Format` and replaces older format
    /// strings with the current format string.
    pub fn fix(&mut self) {
        if self.0.contains_key("Format-Specification") {
            self.0.rename("Format-Specification", "Format");
        }

        if let Some(mut format) = self.0.get("Format") {
            if !format.ends_with('/') {
                format.push('/');
            }

            if let Some(rest) = format.strip_prefix("http:") {
                format = format!("https:{}", rest);
            }

            if KNOWN_FORMATS.contains(&format.as_str()) {
                format = CURRENT_FORMAT.to_string();
            }

            self.0.set("Format", format.as_str());
        }
    }
}

/// A files paragraph
pub struct FilesParagraph(Paragraph);

impl FilesParagraph {
    /// List of file patterns in the paragraph
    pub fn files(&self) -> Vec<String> {
        self.0
            .get("Files")
            .unwrap()
            .split_whitespace()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
    }

    /// Check whether the paragraph matches the given filename
    pub fn matches(&self, filename: &std::path::Path) -> bool {
        self.files()
            .iter()
            .any(|f| crate::glob::glob_to_regex(f).is_match(filename.to_str().unwrap()))
    }

    /// Copyright holders in the paragraph
    pub fn copyright(&self) -> Vec<String> {
        self.0
            .get("Copyright")
            .unwrap_or_default()
            .split('\n')
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
    }

    /// Set the copyright
    pub fn set_copyright(&mut self, authors: &[&str]) {
        self.0
            .set_with_field_order("Copyright", &authors.join("\n"), FILES_FIELD_ORDER);
    }

    /// Comment associated with the files paragraph
    pub fn comment(&self) -> Option<String> {
        self.0.get("Comment")
    }

    /// Set the comment associated with the files paragraph
    pub fn set_comment(&mut self, comment: &str) {
        self.0
            .set_with_field_order("Comment", comment, FILES_FIELD_ORDER);
    }

    /// License in the paragraph
    pub fn license(&self) -> Option<License> {
        self.0.get("License").map(|x| {
            x.split_once('\n').map_or_else(
                || License::Name(x.to_string()),
                |(name, text)| {
                    if name.is_empty() {
                        License::Text(text.to_string())
                    } else {
                        License::Named(name.to_string(), text.to_string())
                    }
                },
            )
        })
    }

    /// Set the license associated with the files paragraph
    pub fn set_license(&mut self, license: &License) {
        let text = match license {
            License::Name(name) => name.to_string(),
            License::Named(name, text) => format!("{}\n{}", name, text),
            License::Text(text) => text.to_string(),
        };
        self.0
            .set_with_field_order("License", &text, FILES_FIELD_ORDER);
    }
}

/// A paragraph that contains a license
pub struct LicenseParagraph(Paragraph);

impl From<LicenseParagraph> for License {
    fn from(p: LicenseParagraph) -> Self {
        let x = p.0.get("License").unwrap();
        x.split_once('\n').map_or_else(
            || License::Name(x.to_string()),
            |(name, text)| {
                if name.is_empty() {
                    License::Text(text.to_string())
                } else {
                    License::Named(name.to_string(), text.to_string())
                }
            },
        )
    }
}

impl LicenseParagraph {
    /// Comment associated with the license
    pub fn comment(&self) -> Option<String> {
        self.0.get("Comment")
    }

    /// Set the comment associated with the license
    pub fn set_comment(&mut self, comment: &str) {
        self.0
            .set_with_field_order("Comment", comment, LICENSE_FIELD_ORDER);
    }

    /// Name of the license
    pub fn name(&self) -> Option<String> {
        self.0
            .get("License")
            .and_then(|x| x.split_once('\n').map(|(name, _)| name.to_string()))
    }

    /// Text of the license
    pub fn text(&self) -> Option<String> {
        self.0
            .get("License")
            .and_then(|x| x.split_once('\n').map(|(_, text)| text.to_string()))
    }

    /// Set the license
    pub fn set_license(&mut self, license: &License) {
        let text = match license {
            License::Name(name) => name.to_string(),
            License::Named(name, text) => format!("{}\n{}", name, text),
            License::Text(text) => text.to_string(),
        };
        self.0
            .set_with_field_order("License", &text, LICENSE_FIELD_ORDER);
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_not_machine_readable() {
        let s = r#"
This copyright file is not machine readable.
"#;
        let ret = s.parse::<super::Copyright>();
        assert!(ret.is_err());
        assert!(matches!(ret.unwrap_err(), super::Error::NotMachineReadable));
    }

    #[test]
    fn test_new() {
        let n = super::Copyright::new();
        assert_eq!(
            n.to_string().as_str(),
            "Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/\n"
        );
    }

    #[test]
    fn test_parse() {
        let s = r#"Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/
Upstream-Name: foo
Upstream-Contact: Joe Bloggs <joe@example.com>
Source: https://example.com/foo

Files: *
Copyright:
  2020 Joe Bloggs <joe@example.com>
License: GPL-3+

Files: debian/*
Comment: Debian packaging is licensed under the GPL-3+.
Copyright: 2023 Jelmer Vernooij
License: GPL-3+

License: GPL-3+
 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.
"#;
        let copyright = s.parse::<super::Copyright>().expect("failed to parse");

        assert_eq!(
            "https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/",
            copyright.header().unwrap().format_string().unwrap()
        );
        assert_eq!("foo", copyright.header().unwrap().upstream_name().unwrap());
        assert_eq!(
            "Joe Bloggs <joe@example.com>",
            copyright.header().unwrap().upstream_contact().unwrap()
        );
        assert_eq!(
            "https://example.com/foo",
            copyright.header().unwrap().source().unwrap()
        );

        let files = copyright.iter_files().collect::<Vec<_>>();
        assert_eq!(2, files.len());
        assert_eq!("*", files[0].files().join(" "));
        assert_eq!("debian/*", files[1].files().join(" "));
        assert_eq!(
            "Debian packaging is licensed under the GPL-3+.",
            files[1].comment().unwrap()
        );
        assert_eq!(
            vec!["2023 Jelmer Vernooij".to_string()],
            files[1].copyright()
        );
        assert_eq!("GPL-3+", files[1].license().unwrap().name().unwrap());
        assert_eq!(files[1].license().unwrap().text(), None);

        let licenses = copyright.iter_licenses().collect::<Vec<_>>();
        assert_eq!(1, licenses.len());
        assert_eq!("GPL-3+", licenses[0].name().unwrap());
        assert_eq!(
            "This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.",
            licenses[0].text().unwrap()
        );

        let upstream_files = copyright.find_files(std::path::Path::new("foo.c")).unwrap();
        assert_eq!(vec!["*"], upstream_files.files());

        let debian_files = copyright
            .find_files(std::path::Path::new("debian/foo.c"))
            .unwrap();
        assert_eq!(vec!["debian/*"], debian_files.files());

        let gpl = copyright.find_license_by_name("GPL-3+");
        assert!(gpl.is_some());

        let gpl = copyright.find_license_for_file(std::path::Path::new("debian/foo.c"));
        assert_eq!(gpl.unwrap().name().unwrap(), "GPL-3+");
    }

    #[test]
    fn test_from_str_relaxed() {
        let s = r#"Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/
Upstream-Name: foo
Source: https://example.com/foo

Files: *
Copyright: 2020 Joe Bloggs <joe@example.com>
License: GPL-3+
"#;
        let (copyright, errors) = super::Copyright::from_str_relaxed(s).unwrap();
        assert!(errors.is_empty());
        assert_eq!("foo", copyright.header().unwrap().upstream_name().unwrap());
    }

    #[test]
    fn test_from_file_relaxed() {
        let tmpfile = std::env::temp_dir().join("test_copyright.txt");
        std::fs::write(
            &tmpfile,
            r#"Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/
Upstream-Name: foo
Source: https://example.com/foo

Files: *
Copyright: 2020 Joe Bloggs <joe@example.com>
License: GPL-3+
"#,
        )
        .unwrap();
        let (copyright, errors) = super::Copyright::from_file_relaxed(&tmpfile).unwrap();
        assert!(errors.is_empty());
        assert_eq!("foo", copyright.header().unwrap().upstream_name().unwrap());
        std::fs::remove_file(&tmpfile).unwrap();
    }

    #[test]
    fn test_header_set_upstream_contact() {
        let copyright = super::Copyright::new();
        let mut header = copyright.header().unwrap();
        header.set_upstream_contact("Test Person <test@example.com>");
        assert_eq!(
            header.upstream_contact().unwrap(),
            "Test Person <test@example.com>"
        );
    }

    #[test]
    fn test_header_set_source() {
        let copyright = super::Copyright::new();
        let mut header = copyright.header().unwrap();
        header.set_source("https://example.com/source");
        assert_eq!(header.source().unwrap(), "https://example.com/source");
    }

    #[test]
    fn test_license_paragraph_set_comment() {
        let s = r#"Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/

License: GPL-3+
 This is the license text.
"#;
        let copyright = s.parse::<super::Copyright>().unwrap();
        let mut license = copyright.iter_licenses().next().unwrap();
        license.set_comment("This is a test comment");
        assert_eq!(license.comment().unwrap(), "This is a test comment");
    }

    #[test]
    fn test_license_paragraph_set_license() {
        let s = r#"Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/

License: GPL-3+
 Old license text.
"#;
        let copyright = s.parse::<super::Copyright>().unwrap();
        let mut license = copyright.iter_licenses().next().unwrap();

        let new_license = crate::License::Named(
            "MIT".to_string(),
            "Permission is hereby granted...".to_string(),
        );
        license.set_license(&new_license);

        assert_eq!(license.name().unwrap(), "MIT");
        assert_eq!(license.text().unwrap(), "Permission is hereby granted...");
    }

    #[test]
    fn test_iter_licenses_excludes_header() {
        // Test that iter_licenses does not include the header paragraph even if it has a License field
        let s = r#"Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/
Upstream-Name: foo
License: GPL-3+

Files: *
Copyright: 2020 Joe Bloggs
License: MIT

License: GPL-3+
 This is the GPL-3+ license text.
"#;
        let copyright = s.parse::<super::Copyright>().unwrap();
        let licenses: Vec<_> = copyright.iter_licenses().collect();

        // Should only have the standalone License paragraph, not the header
        assert_eq!(1, licenses.len());
        assert_eq!("GPL-3+", licenses[0].name().unwrap());
        assert_eq!(
            "This is the GPL-3+ license text.",
            licenses[0].text().unwrap()
        );
    }

    #[test]
    fn test_add_files() {
        let mut copyright = super::Copyright::new();
        let license = crate::License::Name("GPL-3+".to_string());
        copyright.add_files(
            &["src/*", "*.rs"],
            &["2024 John Doe", "2024 Jane Doe"],
            &license,
        );

        let files: Vec<_> = copyright.iter_files().collect();
        assert_eq!(1, files.len());
        assert_eq!(vec!["src/*", "*.rs"], files[0].files());
        assert_eq!(vec!["2024 John Doe", "2024 Jane Doe"], files[0].copyright());
        assert_eq!("GPL-3+", files[0].license().unwrap().name().unwrap());

        // Verify the generated format
        assert_eq!(
            copyright.to_string(),
            "Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/\n\n\
             Files: src/* *.rs\n\
             Copyright: 2024 John Doe\n           2024 Jane Doe\n\
             License: GPL-3+\n"
        );
    }

    #[test]
    fn test_add_files_with_license_text() {
        let mut copyright = super::Copyright::new();
        let license = crate::License::Named(
            "MIT".to_string(),
            "Permission is hereby granted...".to_string(),
        );
        copyright.add_files(&["*"], &["2024 Test Author"], &license);

        let files: Vec<_> = copyright.iter_files().collect();
        assert_eq!(1, files.len());
        assert_eq!("MIT", files[0].license().unwrap().name().unwrap());
        assert_eq!(
            "Permission is hereby granted...",
            files[0].license().unwrap().text().unwrap()
        );

        // Verify the generated format
        assert_eq!(
            copyright.to_string(),
            "Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/\n\n\
             Files: *\n\
             Copyright: 2024 Test Author\n\
             License: MIT\n         Permission is hereby granted...\n"
        );
    }

    #[test]
    fn test_add_license() {
        let mut copyright = super::Copyright::new();
        let license = crate::License::Named(
            "GPL-3+".to_string(),
            "This is the GPL-3+ license text.".to_string(),
        );
        copyright.add_license(&license);

        let licenses: Vec<_> = copyright.iter_licenses().collect();
        assert_eq!(1, licenses.len());
        assert_eq!("GPL-3+", licenses[0].name().unwrap());
        assert_eq!(
            "This is the GPL-3+ license text.",
            licenses[0].text().unwrap()
        );

        // Verify the generated format
        assert_eq!(
            copyright.to_string(),
            "Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/\n\n\
             License: GPL-3+\n         This is the GPL-3+ license text.\n"
        );
    }

    #[test]
    fn test_add_multiple_paragraphs() {
        let mut copyright = super::Copyright::new();

        // Add a files paragraph
        let license1 = crate::License::Name("MIT".to_string());
        copyright.add_files(&["src/*"], &["2024 Author One"], &license1);

        // Add another files paragraph
        let license2 = crate::License::Name("GPL-3+".to_string());
        copyright.add_files(&["debian/*"], &["2024 Author Two"], &license2);

        // Add a license paragraph
        let license3 =
            crate::License::Named("GPL-3+".to_string(), "Full GPL-3+ text here.".to_string());
        copyright.add_license(&license3);

        // Verify all paragraphs were added
        assert_eq!(2, copyright.iter_files().count());
        assert_eq!(1, copyright.iter_licenses().count());

        let files: Vec<_> = copyright.iter_files().collect();
        assert_eq!(vec!["src/*"], files[0].files());
        assert_eq!(vec!["debian/*"], files[1].files());

        let licenses: Vec<_> = copyright.iter_licenses().collect();
        assert_eq!("GPL-3+", licenses[0].name().unwrap());
        assert_eq!("Full GPL-3+ text here.", licenses[0].text().unwrap());

        // Verify the generated format
        assert_eq!(
            copyright.to_string(),
            "Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/\n\n\
             Files: src/*\n\
             Copyright: 2024 Author One\n\
             License: MIT\n\n\
             Files: debian/*\n\
             Copyright: 2024 Author Two\n\
             License: GPL-3+\n\n\
             License: GPL-3+\n         Full GPL-3+ text here.\n"
        );
    }

    #[test]
    fn test_remove_license_by_name() {
        let mut copyright = super::Copyright::new();

        // Add multiple license paragraphs
        let license1 = crate::License::Named("MIT".to_string(), "MIT license text.".to_string());
        copyright.add_license(&license1);

        let license2 =
            crate::License::Named("GPL-3+".to_string(), "GPL-3+ license text.".to_string());
        copyright.add_license(&license2);

        let license3 =
            crate::License::Named("Apache-2.0".to_string(), "Apache license text.".to_string());
        copyright.add_license(&license3);

        // Verify we have 3 license paragraphs
        assert_eq!(3, copyright.iter_licenses().count());

        // Remove the GPL-3+ license
        let removed = copyright.remove_license_by_name("GPL-3+");
        assert!(removed);

        // Verify we now have 2 license paragraphs
        assert_eq!(2, copyright.iter_licenses().count());

        // Verify the remaining licenses
        let licenses: Vec<_> = copyright.iter_licenses().collect();
        assert_eq!("MIT", licenses[0].name().unwrap());
        assert_eq!("Apache-2.0", licenses[1].name().unwrap());

        // Try to remove a non-existent license
        let removed = copyright.remove_license_by_name("BSD-3-Clause");
        assert!(!removed);
        assert_eq!(2, copyright.iter_licenses().count());
    }

    #[test]
    fn test_remove_files_by_pattern() {
        let mut copyright = super::Copyright::new();

        // Add multiple files paragraphs
        let license1 = crate::License::Name("MIT".to_string());
        copyright.add_files(&["src/*"], &["2024 Author One"], &license1);

        let license2 = crate::License::Name("GPL-3+".to_string());
        copyright.add_files(&["debian/*"], &["2024 Author Two"], &license2);

        let license3 = crate::License::Name("Apache-2.0".to_string());
        copyright.add_files(&["docs/*"], &["2024 Author Three"], &license3);

        // Verify we have 3 files paragraphs
        assert_eq!(3, copyright.iter_files().count());

        // Remove the debian/* files paragraph
        let removed = copyright.remove_files_by_pattern("debian/*");
        assert!(removed);

        // Verify we now have 2 files paragraphs
        assert_eq!(2, copyright.iter_files().count());

        // Verify the remaining files paragraphs
        let files: Vec<_> = copyright.iter_files().collect();
        assert_eq!(vec!["src/*"], files[0].files());
        assert_eq!(vec!["docs/*"], files[1].files());

        // Try to remove a non-existent pattern
        let removed = copyright.remove_files_by_pattern("tests/*");
        assert!(!removed);
        assert_eq!(2, copyright.iter_files().count());
    }

    #[test]
    fn test_remove_files_by_pattern_with_multiple_patterns() {
        let mut copyright = super::Copyright::new();

        // Add a files paragraph with multiple patterns
        let license = crate::License::Name("MIT".to_string());
        copyright.add_files(&["src/*", "*.rs"], &["2024 Author"], &license);

        // Verify we have 1 files paragraph
        assert_eq!(1, copyright.iter_files().count());

        // Remove by matching one of the patterns
        let removed = copyright.remove_files_by_pattern("*.rs");
        assert!(removed);

        // Verify the paragraph was removed
        assert_eq!(0, copyright.iter_files().count());
    }

    #[test]
    fn test_field_order_with_comment() {
        // Test that fields follow DEP-5 order: Files, Copyright, License, Comment
        let mut copyright = super::Copyright::new();

        let files = vec!["*"];
        let copyrights = vec!["Unknown"];
        let license = crate::License::Name("GPL-2+".to_string());

        let mut para = copyright.add_files(&files, &copyrights, &license);
        para.set_comment("Test comment");

        let output = copyright.to_string();

        // Expected order: Format, blank line, Files, Copyright, License, Comment
        let expected =
            "Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/\n\n\
                        Files: *\n\
                        Copyright: Unknown\n\
                        License: GPL-2+\n\
                        Comment: Test comment\n";

        assert_eq!(
            output, expected,
            "Fields should be in DEP-5 order (Files, Copyright, License, Comment), but got:\n{}",
            output
        );
    }
}
