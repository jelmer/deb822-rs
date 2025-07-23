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
    /// Problem with URL validity
    URLParsingFailure(url::ParseError),
}

impl From<std::io::Error> for RepositoryError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}

impl From<url::ParseError> for RepositoryError {
    fn from(e: url::ParseError) -> Self {
        Self::URLParsingFailure(e)
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
            Self::URLParsingFailure(e) => write!(f, "URL parsing failure: {}", e),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_repository_error_display() {
        // Test each error variant
        assert_eq!(
            RepositoryError::InvalidFormat.to_string(),
            "Invalid repository format"
        );
        assert_eq!(
            RepositoryError::InvalidUri.to_string(),
            "Invalid repository URI"
        );
        assert_eq!(
            RepositoryError::MissingUri.to_string(),
            "Missing repository URI"
        );
        assert_eq!(
            RepositoryError::InvalidType.to_string(),
            "Invalid repository type"
        );
        assert_eq!(
            RepositoryError::InvalidSignature.to_string(),
            "The field `Signed-By` is incorrect"
        );

        // Test lossy error
        let lossy_err = deb822_fast::Error::UnexpectedEof;
        let repo_err = RepositoryError::Lossy(lossy_err);
        assert!(repo_err.to_string().contains("Lossy parser error:"));

        // Test IO error
        let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
        let repo_err = RepositoryError::from(io_err);
        assert!(repo_err.to_string().contains("IO error:"));
    }
}
