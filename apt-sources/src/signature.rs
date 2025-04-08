//! A module implementing `Signature` type that holds info about variants of the signature key used by the repository

use std::path::PathBuf;

use crate::error::RepositoryError;

/// A type to store
#[derive(Debug, PartialEq, Clone)]
pub enum Signature {
    /// The PGP key is stored inside the `.sources` files
    KeyBlock(String), // TODO: shall we validate PGP Public Key?
    /// The public key is store in a file of the given path
    KeyPath(PathBuf), // TODO: man page specifies fingerprints, but there's no example
}

impl std::str::FromStr for Signature {
    type Err = RepositoryError;

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        // Normal examples say PGP line shall start next line after `Signed-By` field
        // but all my files have it starting after a space in the same line and that works.
        // It's quite confusing, but let it be... we have to deal with reality.
        if text.contains("\n") {
            // If text is multiline, we assume PGP Public Key block
            Ok(Signature::KeyBlock(text.to_string()))
        } else {
            // otherwise one-liner is a path
            Ok(Signature::KeyPath(text.into()))
        }

        // if let Some((name, rest)) = text.split_once('\n') {
        //     if name.is_empty() {
        //         println!("& Name = {}", name);
        //         Ok(Signature::KeyBlock(rest.to_string()))
        //     } else {
        //         println!("& Name = {}", name);
        //         Err(RepositoryError::InvalidSignature)
        //     }
        // } else {
        //     println!("& No name");
        //     Ok(Signature::KeyPath(text.into()))
        // }
    }
}

impl std::fmt::Display for Signature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Signature::KeyBlock(text) => write!(f, "\n{}", text),
            Signature::KeyPath(path) => f.write_str(path.to_string_lossy().as_ref()),
        }
    }
}
