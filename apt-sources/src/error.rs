//! A module for handling errors in `apt-sources` crate of `deb822-rs` project.
//! It intends to address error handling in meaningful manner, less vague than just passing
//! `String` as error.

/// Errors for APT sources parsing and conversion to/from [`Repository`] or [`LegacyRepository`]
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
    /// The Yes/No/Force field has invalid/unexpected value
    YesNoForceFieldInvalid,
    /// The Yes/No field has invalid/unexpected value
    YesNoFieldInvalid,
    /// The field in the parsed data is not recognized (check `man sources.list`)
    UnrecognizedFieldName(String),
    /// Errors in lossy serializer or deserializer
    Lossy(deb822_fast::Error),
    /// I/O Error
    Io(std::io::Error),
    /// URL Error
    Url(url::ParseError),
}

/// Errors that can occur when loading repositories from directories
#[derive(Debug)]
pub enum LoadError {
    /// Failed to read a file
    Io {
        /// The path that failed to be read
        path: std::path::PathBuf,
        /// The underlying I/O error
        error: std::io::Error,
    },
    /// Failed to parse a file
    Parse {
        /// The path that failed to be parsed
        path: std::path::PathBuf,
        /// The parsing error message
        error: String,
    },
    /// Failed to read directory entries
    DirectoryRead {
        /// The directory path that failed to be read
        path: std::path::PathBuf,
        /// The underlying I/O error
        error: std::io::Error,
    },
}

impl From<std::io::Error> for RepositoryError {
    fn from(e: std::io::Error) -> Self {
        Self::Io(e)
    }
}

impl From<url::ParseError> for RepositoryError {
    fn from(e: url::ParseError) -> Self {
        Self::Url(e)
    }
}

impl std::fmt::Display for RepositoryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        // Spare longer messages to split lines by `rustfmt`
        const YNFERRMSG: &str = "The field requiring only `Yes`/`No`/`Force` values is incorrect";
        const YNERRMSG: &str = "The field requiring only `Yes`/`No` values is incorrect";
        const UFNERRMSG: &str =
            "The field in the parsed data is not recognized (check `man sources.list`)";
        match self {
            Self::InvalidFormat => write!(f, "Invalid repository format"),
            Self::InvalidUri => write!(f, "Invalid repository URI"),
            Self::MissingUri => write!(f, "Missing repository URI"),
            Self::InvalidType => write!(f, "Invalid repository type"),
            Self::InvalidSignature => write!(f, "The field `Signed-By` is incorrect"),
            Self::YesNoForceFieldInvalid => f.write_str(YNFERRMSG),
            Self::YesNoFieldInvalid => f.write_str(YNERRMSG),
            Self::UnrecognizedFieldName(_) => f.write_str(UFNERRMSG), // TODO: dump the field name
            Self::Lossy(e) => write!(f, "Lossy parser error: {e}"),
            Self::Io(e) => write!(f, "IO error: {e}"),
            Self::Url(e) => write!(f, "URL parse error: {e}"),
        }
    }
}

impl std::fmt::Display for LoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Io { path, error } => write!(f, "Failed to read {}: {}", path.display(), error),
            Self::Parse { path, error } => {
                write!(f, "Failed to parse {}: {}", path.display(), error)
            }
            Self::DirectoryRead { path, error } => {
                write!(f, "Failed to read directory {}: {}", path.display(), error)
            }
        }
    }
}

impl std::error::Error for RepositoryError {}
impl std::error::Error for LoadError {}

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

    #[test]
    fn test_load_error_display() {
        use std::path::PathBuf;

        // Test IO error
        let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
        let load_err = LoadError::Io {
            path: PathBuf::from("/test/path"),
            error: io_err,
        };
        assert!(load_err.to_string().contains("Failed to read /test/path"));
        assert!(load_err.to_string().contains("file not found"));

        // Test Parse error
        let parse_err = LoadError::Parse {
            path: PathBuf::from("/test/file.list"),
            error: "Invalid format".to_string(),
        };
        assert!(parse_err
            .to_string()
            .contains("Failed to parse /test/file.list"));
        assert!(parse_err.to_string().contains("Invalid format"));

        // Test DirectoryRead error
        let dir_err = std::io::Error::new(std::io::ErrorKind::PermissionDenied, "access denied");
        let load_err = LoadError::DirectoryRead {
            path: PathBuf::from("/test/dir"),
            error: dir_err,
        };
        assert!(load_err
            .to_string()
            .contains("Failed to read directory /test/dir"));
        assert!(load_err.to_string().contains("access denied"));
    }
}
