#![deny(missing_docs)]
//! A library for parsing and manipulating debian/copyright files that
//! use the DEP-5 format.
//!
//! # Examples
//!
//! ```rust
//!
//! use debian_copyright::Copyright;
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
//!
//! See the ``edit`` module (behind the ``edit`` feature) for a more forgiving parser that
//! allows partial parsing, parsing files with errors and unknown fields and editing while
//! preserving formatting.

#[cfg(feature = "edit")]
pub mod edit;

#[cfg(feature = "lossless")]
#[deprecated(since = "0.1.28", note = "Use `edit` module instead")]
pub mod lossless {
    //! Deprecated: Use the `edit` module instead.
    pub use crate::edit::*;
}
pub mod lossy;
pub use lossy::Copyright;

/// The current version of the DEP-5 format.
pub const CURRENT_FORMAT: &str =
    "https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/";

/// The known versions of the DEP-5 format.
pub const KNOWN_FORMATS: &[&str] = &[CURRENT_FORMAT];

mod glob;

/// A license, which can be just a name, a text or a named license.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum License {
    /// A license with just a name.
    Name(String),

    /// A license with just a text.
    Text(String),

    /// A license with a name and a text.
    Named(String, String),
}

impl License {
    /// Returns the name of the license, if any.
    pub fn name(&self) -> Option<&str> {
        match self {
            License::Name(name) => Some(name),
            License::Text(_) => None,
            License::Named(name, _) => Some(name),
        }
    }

    /// Returns the text of the license, if any.
    pub fn text(&self) -> Option<&str> {
        match self {
            License::Name(_) => None,
            License::Text(text) => Some(text),
            License::Named(_, text) => Some(text),
        }
    }
}

impl std::str::FromStr for License {
    type Err = String;

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        if let Some((name, rest)) = text.split_once('\n') {
            if name.is_empty() {
                Ok(License::Text(rest.to_string()))
            } else {
                Ok(License::Named(name.to_string(), rest.to_string()))
            }
        } else {
            Ok(License::Name(text.to_string()))
        }
    }
}

impl std::fmt::Display for License {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            License::Name(name) => f.write_str(name),
            License::Text(text) => write!(f, "\n{}", text),
            License::Named(name, text) => write!(f, "{}\n{}", name, text),
        }
    }
}
