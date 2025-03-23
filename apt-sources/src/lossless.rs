//! This optional module adds feature of lossless handling of the APT repositories,
//! meaning changes retain structure and unprocessed data of sources, like comments.
//!
//! Use either lossy or lossless as you see fit, both serve the same purpose, but
//! with different trade-offs.
//!
//! # Examples
//! ```rust
//! use apt_sources::{lossless::Repositories, traits::Repository};
//! use std::path::Path;
//!
//! let text = r#"# Example repository sources for APT
//! Types: deb
//! URIs: http://ports.ubuntu.com/
//! Suites: noble
//! Components: stable
//! Architectures: arm64
//! Signed-By:
//!  -----BEGIN PGP PUBLIC KEY BLOCK-----
//!  .
//!  mDMEY865UxYJKwYBBAHaRw8BAQdAd7Z0srwuhlB6JKFkcf4HU4SSS/xcRfwEQWzr
//!  crf6AEq0SURlYmlhbiBTdGFibGUgUmVsZWFzZSBLZXkgKDEyL2Jvb2t3b3JtKSA8
//!  ZGViaWFuLXJlbGVhc2VAbGlzdHMuZGViaWFuLm9yZz6IlgQTFggAPhYhBE1k/sEZ
//!  wgKQZ9bnkfjSWFuHg9SBBQJjzrlTAhsDBQkPCZwABQsJCAcCBhUKCQgLAgQWAgMB
//!  Ah4BAheAAAoJEPjSWFuHg9SBSgwBAP9qpeO5z1s5m4D4z3TcqDo1wez6DNya27QW
//!  WoG/4oBsAQCEN8Z00DXagPHbwrvsY2t9BCsT+PgnSn9biobwX7bDDg==
//!  =5NZE
//!  -----END PGP PUBLIC KEY BLOCK-----"#;
//!
//! let r = text.parse::<Repositories>().unwrap();
//! let r = r.repositories().nth(0).unwrap();
//! let suites = r.suites();
//! assert_eq!(suites[0], "noble");
//! ```

use std::{borrow::Cow, collections::HashSet, str::FromStr};

use deb822_lossless::{Deb822, Paragraph};
use url::Url;

use crate::{error::RepositoryError, signature::Signature, traits, RepositoryType};

/// A structure representing APT repository as declared by DEB822 source file,
/// this is slower lossless variant (retaining unsupported fields and comments).
#[derive(PartialEq)]
pub struct Repository(Paragraph);

impl Repository {
    fn return_string_array_cow(&self, key: &str) -> Cow<'_, [String]> {
        Cow::Owned(
            self.0
                .get(key)
                .unwrap() // TODO: type is mandatory, but this is lazy evaluation, that normally would fail deserialization
                .split_whitespace()
                .map(|s| s.to_owned())
                .collect(),
        )
    }

    fn return_optional_yes_no(&self, key: &str) -> Option<bool> {
        self.0
            .get(key)
            .map_or(None, |v| super::deserialize_yesno(&v).ok()) // TODO: error consumed!
    }
}

impl traits::Repository for Repository {
    fn enabled(&self) -> bool {
        self.0.get("Enabled").is_none_or(|x| x == "yes")
    }

    fn types(&self) -> Cow<'_, std::collections::HashSet<crate::RepositoryType>> {
        Cow::Owned(
            self.0
                .get("Types")
                .unwrap() // TODO: type is mandatory, be this is lazy evaluation, that normally would fail deserialization
                .split_whitespace()
                .map(|t| RepositoryType::from_str(t))
                .collect::<Result<HashSet<RepositoryType>, RepositoryError>>()
                .unwrap(),
        ) // TODO: incorrect values would normally fail deserialization
    }

    fn uris(&self) -> Cow<'_, [url::Url]> {
        Cow::Owned(
            self.0
                .get("URIs")
                .unwrap() // TODO: type is mandatory, but this is lazy evaluation, that normally would fail deserialization
                .split_whitespace()
                .map(|u| Url::from_str(u))
                .collect::<Result<Vec<Url>, url::ParseError>>()
                .unwrap(),
        )
    }

    fn suites(&self) -> Cow<'_, [String]> {
        self.return_string_array_cow("Suites")
    }

    fn components(&self) -> Cow<'_, [String]> {
        self.return_string_array_cow("Components")
    }

    fn architectures(&self) -> Cow<'_, [String]> {
        self.return_string_array_cow("Architectures")
    }

    fn languages(&self) -> Cow<'_, [String]> {
        self.return_string_array_cow("Languages")
    }

    fn targets(&self) -> Cow<'_, [String]> {
        self.return_string_array_cow("Targets")
    }

    fn pdiffs(&self) -> Option<bool> {
        self.return_optional_yes_no("PDiffs")
    }

    fn by_hash(&self) -> Option<crate::YesNoForce> {
        self.0
            .get("By-Hash")
            .map_or(None, |v| super::YesNoForce::from_str(&v).ok()) // TODO: error consumed! (quietly ignored if values don't match)
    }

    fn allow_insecure(&self) -> Option<bool> {
        self.return_optional_yes_no("Allow-Insecure")
    }

    fn allow_weak(&self) -> Option<bool> {
        self.return_optional_yes_no("Allow-Weak")
    }

    fn allow_downgrade_to_insecure(&self) -> Option<bool> {
        self.return_optional_yes_no("Allow-Downgrade-To-Insecure")
    }

    fn trusted(&self) -> Option<bool> {
        self.return_optional_yes_no("Trusted")
    }

    fn signature(&self) -> Option<Cow<'_, crate::signature::Signature>> {
        self.0
            .get("Signed-By")
            .and_then(|v| Signature::from_str(&v).ok()) // TODO: another case of errors in late parsing
            .and_then(|s| Some(Cow::Owned(s)))

        //filter(|v| Signature::from_str(&v).ok().and_then(|s| Cow::Owned(s)))
    }

    fn x_repolib_name(&self) -> Option<Cow<'_, str>> {
        self.0.get("X-Repolib-Name").map(|x| Cow::Owned(x))
    }

    fn description(&self) -> Option<Cow<'_, str>> {
        self.0.get("Description").map(|x| Cow::Owned(x))
    }
}

/// Container for multiple `Repository` specifications as single `.sources` file may contain as per specification
#[derive(Debug)]
pub struct Repositories(Deb822);

impl Repositories {
    /// Provides iterator over individual repositories in the whole file
    pub fn repositories(&self) -> impl Iterator<Item = Repository> {
        // TODO: repository is _a copy_ of the paragraph! not compatible with lossy
        self.0.paragraphs().filter_map(|p| Some(Repository(p)))
    }
}

impl std::str::FromStr for Repositories {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let deb822: deb822_lossless::Deb822 = s
            .parse()
            .map_err(|e: deb822_lossless::ParseError| e.to_string())?;

        //let repos = deb822.paragraphs().map(|p| Repository::from_paragraph(&p)).collect::<Result<Vec<Repository>, Self::Err>>()?;
        Ok(Repositories(deb822))
    }
}

// TODO: this cannot be easily implemented to act like in `Vec<>` as we don't have slices of `Paragraph`s mapped into `Repository`s
// impl<Idx> Index<Idx> for Repositories
// where
//     Idx: SliceIndex<[Repository], Output = Repository>
// {
//     type Output = Idx::Output;

//     #[inline(always)]
//     fn index(&self, index: Idx) -> &Self::Output {
//         self.0.paragraphs().nth(index).expect("Index out of bounds").into()
//     }
// }

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use indoc::indoc;

    use crate::signature::Signature;
    use crate::traits::Repository as RepositoryTrait;

    use super::Repositories;

    #[test]
    fn test_not_machine_readable() {
        let s = indoc!(
            r#"
            deb [arch=arm64 signed-by=/usr/share/keyrings/docker.gpg] http://ports.ubuntu.com/ noble stable
        "#
        );
        let ret = s.parse::<Repositories>();
        assert!(ret.is_err());
        //assert_eq!(ret.unwrap_err(), "Not machine readable".to_string());
        assert_eq!(
            ret.unwrap_err(),
            "expected ':', got Some(NEWLINE)\n".to_owned()
        );
    }

    #[test]
    fn test_parse_trivial() {
        let s = indoc!(
            r#"
            Types: deb
            URIs: https://ports.ubuntu.com/
            Suites: jammy
            Components: main restricted universe multiverse
        "#
        );

        let repos = s
            .parse::<Repositories>()
            .expect("Shall be parsed flawlessly");
        let only_repo = repos
            .repositories()
            .next()
            .expect("Failed to pick only repo");
        assert!(only_repo.types().contains(&super::RepositoryType::Binary));
        assert_eq!(
            only_repo.components().as_ref(),
            [
                "main".to_owned(),
                "restricted".to_owned(),
                "universe".to_owned(),
                "multiverse".to_owned()
            ]
        );
    }

    #[test]
    fn test_parse_w_keyblock() {
        let s = indoc!(
            r#"
            Types: deb
            URIs: http://ports.ubuntu.com/
            Suites: noble
            Components: stable
            Architectures: arm64
            Signed-By:
             -----BEGIN PGP PUBLIC KEY BLOCK-----
             .
             mDMEY865UxYJKwYBBAHaRw8BAQdAd7Z0srwuhlB6JKFkcf4HU4SSS/xcRfwEQWzr
             crf6AEq0SURlYmlhbiBTdGFibGUgUmVsZWFzZSBLZXkgKDEyL2Jvb2t3b3JtKSA8
             ZGViaWFuLXJlbGVhc2VAbGlzdHMuZGViaWFuLm9yZz6IlgQTFggAPhYhBE1k/sEZ
             wgKQZ9bnkfjSWFuHg9SBBQJjzrlTAhsDBQkPCZwABQsJCAcCBhUKCQgLAgQWAgMB
             Ah4BAheAAAoJEPjSWFuHg9SBSgwBAP9qpeO5z1s5m4D4z3TcqDo1wez6DNya27QW
             WoG/4oBsAQCEN8Z00DXagPHbwrvsY2t9BCsT+PgnSn9biobwX7bDDg==
             =5NZE
             -----END PGP PUBLIC KEY BLOCK-----
        "#
        );

        let repos = s
            .parse::<Repositories>()
            .expect("Shall be parsed flawlessly");
        let only_repo = repos
            .repositories()
            .nth(0)
            .expect("Failed to pick only repo");
        assert!(only_repo.types().contains(&super::RepositoryType::Binary));
        assert!(matches!(
            only_repo.signature().expect("Failed to get Signature"),
            Cow::Owned(Signature::KeyBlock(_))
        ));
    }

    #[test]
    fn test_parse_w_keypath() {
        let s = indoc!(
            r#"
            Types: deb
            URIs: http://ports.ubuntu.com/
            Suites: noble
            Components: stable
            Architectures: arm64
            Signed-By: /usr/share/keyrings/ubuntu-archive-keyring.gpg
        "#
        );

        let repos = s
            .parse::<Repositories>()
            .expect("Shall be parsed flawlessly");
        let only_repo = repos
            .repositories()
            .nth(0)
            .expect("Failed to pick only repo");
        assert!(only_repo.types().contains(&super::RepositoryType::Binary));
        assert!(matches!(
            only_repo
                .signature()
                .expect("Failed to get Signature")
                .as_ref(),
            Signature::KeyPath(_)
        ));
    }

    // #[test]
    // fn test_serialize() {
    //     //let repos = Repositories::empty();
    //     let repos = Repositories::new([
    //         Repository {
    //             enabled: Some(true), // TODO: looks odd, as only `Enabled: no` in meaningful
    //             types: HashSet::from([RepositoryType::Binary]),
    //             architectures: Some(vec!["arm64".to_owned()]),
    //             uris: vec![Url::from_str("https://deb.debian.org/debian").unwrap()],
    //             suites: vec!["jammy".to_owned()],
    //             components: vec!["main". to_owned()],
    //             signature: None,
    //             x_repolib_name: None,
    //             languages: None,
    //             targets: None,
    //             pdiffs: None,
    //             ..Default::default()
    //         }
    //     ]);
    //     let text = repos.to_string();
    //     assert_eq!(text, indoc! {r#"
    //         Enabled: yes
    //         Types: deb
    //         URIs: https://deb.debian.org/debian
    //         Suites: jammy
    //         Components: main
    //         Architectures: arm64
    //     "#});
    // }
}
