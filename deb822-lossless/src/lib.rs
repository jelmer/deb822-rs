#![deny(missing_docs)]
#![allow(clippy::type_complexity)]
#![doc = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/README.md"))]
// Until we drop support for PyO3 0.22, allow use of deprecated functions.
#![allow(deprecated)]

mod common;
pub mod convert;
mod lex;
pub mod lossless;
pub mod lossy;
pub use convert::{FromDeb822Paragraph, ToDeb822Paragraph};
#[cfg(feature = "derive")]
pub use deb822_derive::{FromDeb822, ToDeb822};
pub use lossless::{Deb822, Error, Paragraph, ParseError};

/// The indentation to use when writing a deb822 file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Indentation {
    /// Use the same indentation as the original line for the value.
    FieldNameLength,

    /// The number of spaces to use for indentation.
    Spaces(u32),
}

impl Default for Indentation {
    fn default() -> Self {
        Indentation::Spaces(4)
    }
}
