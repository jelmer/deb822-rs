//! A module for handling errors in `apt-sources` crate of `deb822-rs` project.
//! It intends to address error handling in meaningful manner, less vague than just passing
//! `String` as error.

/// Errors for APT sources parsing and conversion to `Repository`
#[derive(Debug)]
pub enum RepositoryError {
    /// Invalid repository format
    InvalidFormat,
    /// Invalid repository URI
    InvalidUri,
    /// Missing repository URI - mandatory
    MissingUri,
    /// Unrecognized repository type
    InvalidType,
    /// The `Signed-By` field is incorrect
    InvalidSignature,
    /// Errors in lossy serializer or deserializer
    Lossy(deb822_fast::Error),
    /// I/O Error
    Io(std::io::Error),
}

impl From<std::io::Error> for RepositoryError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}

impl std::fmt::Display for RepositoryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::InvalidFormat => write!(f, "Invalid repository format"),
            Self::InvalidUri => write!(f, "Invalid repository URI"),
            Self::MissingUri => write!(f, "Missing repository URI"),
            Self::InvalidType => write!(f, "Invalid repository type"),
            Self::InvalidSignature => write!(f, "The field `Signed-By` is incorrect"),
            Self::Lossy(e) => write!(f, "Lossy parser error: {}", e),
            Self::Io(e) => write!(f, "IO error: {}", e),
        }
    }
}
