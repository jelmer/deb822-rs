//! Lossless parser for various Debian control files
//!
//! This library provides a parser for various Debian control files, such as `control`, `changes`,
//! and apt `Release`, `Packages`, and `Sources` files. The parser is lossless, meaning that it
//! preserves all formatting as well as any possible errors in the files.

pub mod apt;
pub mod buildinfo;
pub mod changes;
pub mod control;
pub mod relations;
pub use control::*;
pub use deb822_lossless::Parse;
pub use relations::*;
