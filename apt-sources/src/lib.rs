#![deny(missing_docs)]
//! A library for parsing and manipulating APT source files that
//! use the DEB822 format to hold package repositories specifications.
//!
//! <div class="warning">
//!
//! Currently only lossy _serialization_ is implemented, lossless support
//! retaining file sequence and comments would come at later date.
//!
//! </div>
//!
//! # Examples
//!
//! ```rust
//!
//! use apt_sources::Repositories;
//! use std::path::Path;
//!
//! let text = r#"Types: deb
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
//! let suites = r[0].suites();
//! assert_eq!(suites[0], "noble");
//! ```
//!
// TODO: Not supported yet:
// See the ``lossless`` module (behind the ``lossless`` feature) for a more forgiving parser that
// allows partial parsing, parsing files with errors and unknown fields and editing while
// preserving formatting.

use deb822_fast::{FromDeb822, FromDeb822Paragraph, ToDeb822, ToDeb822Paragraph};
use error::RepositoryError;
use signature::Signature;
use std::result::Result;
use std::{collections::HashSet, ops::Deref, str::FromStr};
use url::Url;

/// Distribution detection and utilities
pub mod distribution;
pub mod error;
#[cfg(feature = "keyserver")]
pub mod keyserver;
pub mod ppa;
pub mod signature;
/// Module for managing APT source lists
pub mod sources_manager;

/// A representation of the repository type, by role of packages it can provide, either `Binary`
/// (indicated by `deb`) or `Source` (indicated by `deb-src`).
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum RepositoryType {
    /// Repository with binary packages, indicated as `deb`
    Binary,
    /// Repository with source packages, indicated as `deb-src`
    Source,
}

impl FromStr for RepositoryType {
    type Err = RepositoryError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "deb" => Ok(RepositoryType::Binary),
            "deb-src" => Ok(RepositoryType::Source),
            _ => Err(RepositoryError::InvalidType),
        }
    }
}

impl From<&RepositoryType> for String {
    fn from(value: &RepositoryType) -> Self {
        match value {
            RepositoryType::Binary => "deb".to_owned(),
            RepositoryType::Source => "deb-src".to_owned(),
        }
    }
}

impl std::fmt::Display for RepositoryType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            RepositoryType::Binary => "deb",
            RepositoryType::Source => "deb-src",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, PartialEq)]
/// Enumeration for fields like `By-Hash` which have third value of `force`
pub enum YesNoForce {
    /// True
    Yes,
    /// False
    No,
    /// Forced
    Force,
}

impl FromStr for YesNoForce {
    type Err = RepositoryError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "yes" => Ok(Self::Yes),
            "no" => Ok(Self::No),
            "force" => Ok(Self::Force),
            _ => Err(RepositoryError::InvalidType),
        }
    }
}

impl From<&YesNoForce> for String {
    fn from(value: &YesNoForce) -> Self {
        match value {
            YesNoForce::Yes => "yes".to_owned(),
            YesNoForce::No => "no".to_owned(),
            YesNoForce::Force => "force".to_owned(),
        }
    }
}

impl std::fmt::Display for YesNoForce {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            YesNoForce::Yes => "yes",
            YesNoForce::No => "no",
            YesNoForce::Force => "force",
        };
        write!(f, "{}", s)
    }
}

fn deserialize_types(text: &str) -> Result<HashSet<RepositoryType>, RepositoryError> {
    text.split_whitespace()
        .map(RepositoryType::from_str)
        .collect::<Result<HashSet<RepositoryType>, RepositoryError>>()
}

fn serialize_types(files: &HashSet<RepositoryType>) -> String {
    files
        .iter()
        .map(|rt| rt.to_string())
        .collect::<Vec<String>>()
        .join("\n")
}

fn deserialize_uris(text: &str) -> Result<Vec<Url>, String> {
    // TODO: bad error type
    text.split_whitespace()
        .map(Url::from_str)
        .collect::<Result<Vec<Url>, _>>()
        .map_err(|e| e.to_string()) // TODO: bad error type
}

fn serialize_uris(uris: &[Url]) -> String {
    uris.iter()
        .map(|u| u.as_str())
        .collect::<Vec<&str>>()
        .join(" ")
}

fn deserialize_string_chain(text: &str) -> Result<Vec<String>, String> {
    // TODO: bad error type
    Ok(text.split_whitespace().map(|x| x.to_string()).collect())
}

fn deserialize_yesno(text: &str) -> Result<bool, String> {
    // TODO: bad error type
    match text {
        "yes" => Ok(true),
        "no" => Ok(false),
        _ => Err("Invalid value for yes/no field".to_owned()),
    }
}

fn serializer_yesno(value: &bool) -> String {
    if *value {
        "yes".to_owned()
    } else {
        "no".to_owned()
    }
}

fn serialize_string_chain(chain: &[String]) -> String {
    chain.join(" ")
}

/// A structure representing APT repository as declared by DEB822 source file
///
/// According to `sources.list(5)` man pages, only four fields are mandatory:
/// * `Types` either `deb` or/and `deb-src`
/// * `URIs` to repositories holding valid APT structure (unclear if multiple are allowed)
/// * `Suites` usually being distribution codenames
/// * `Component` most of the time `main`, but it's a section of the repository
///
/// The manpage specifies following optional fields
/// * `Enabled`        is a yes/no field, default yes
/// * `Architectures`
/// * `Languages`
/// * `Targets`
/// * `PDiffs`         is a yes/no field
/// * `By-Hash`        is a yes/no/force field
/// * `Allow-Insecure` is a yes/no field, default no
/// * `Allow-Weak`     is a yes/no field, default no
/// * `Allow-Downgrade-To-Insecure` is a yes/no field, default no
/// * `Trusted`        us a yes/no field
/// * `Signed-By`      is either path to the key or PGP key block
/// * `Check-Valid-Until` is a yes/no field
/// * `Valid-Until-Min`
/// * `Valid-Until-Max`
/// * `Check-Date`     is a yes/no field
/// * `Date-Max-Future`
/// * `InRelease-Path` relative path
/// * `Snapshot`       either `enable` or a snapshot ID
///
/// The unit tests of APT use:
/// * `Description`
///
/// The RepoLib tool uses:
/// * `X-Repolib-Name` identifier for own reference, meaningless for APT
///
/// Note: Multivalues `*-Add` & `*-Remove` semantics aren't supported.
#[derive(FromDeb822, ToDeb822, Clone, PartialEq, /*Eq,*/ Debug, Default)]
pub struct Repository {
    /// If `no` (false) the repository is ignored by APT
    #[deb822(field = "Enabled", deserialize_with = deserialize_yesno, serialize_with = serializer_yesno)]
    // TODO: support for `default` if omitted is missing
    pub enabled: Option<bool>,

    /// The value `RepositoryType::Binary` (`deb`) or/and `RepositoryType::Source` (`deb-src`)
    #[deb822(field = "Types", deserialize_with = deserialize_types, serialize_with = serialize_types)]
    pub types: HashSet<RepositoryType>, // consider alternative, closed set
    /// The address of the repository
    #[deb822(field = "URIs", deserialize_with = deserialize_uris, serialize_with = serialize_uris)]
    pub uris: Vec<Url>, // according to Debian that's URI, but this type is more advanced than URI from `http` crate
    /// The distribution name as codename or suite type (like `stable` or `testing`)
    #[deb822(field = "Suites", deserialize_with = deserialize_string_chain, serialize_with = serialize_string_chain)]
    pub suites: Vec<String>,
    /// (Optional) Section of the repository, usually `main`, `contrib` or `non-free`
    /// return `None` if repository is Flat Repository Format (https://wiki.debian.org/DebianRepository/Format#Flat_Repository_Format)
    #[deb822(field = "Components", deserialize_with = deserialize_string_chain, serialize_with = serialize_string_chain)]
    pub components: Option<Vec<String>>,

    /// (Optional) Architectures binaries from this repository run on
    #[deb822(field = "Architectures", deserialize_with = deserialize_string_chain, serialize_with = serialize_string_chain)]
    pub architectures: Vec<String>,
    /// (Optional) Translations support to download
    #[deb822(field = "Languages", deserialize_with = deserialize_string_chain, serialize_with = serialize_string_chain)]
    pub languages: Option<Vec<String>>, // TODO: Option is redundant to empty vectors
    /// (Optional) Download targets to acquire from this source
    #[deb822(field = "Targets", deserialize_with = deserialize_string_chain, serialize_with = serialize_string_chain)]
    pub targets: Option<Vec<String>>,
    /// (Optional) Controls if APT should try PDiffs instead of downloading indexes entirely; if not set defaults to configuration option `Acquire::PDiffs`
    #[deb822(field = "PDiffs", deserialize_with = deserialize_yesno)]
    pub pdiffs: Option<bool>,
    /// (Optional) Controls if APT should try to acquire indexes via a URI constructed from a hashsum of the expected file
    #[deb822(field = "By-Hash")]
    pub by_hash: Option<YesNoForce>,
    /// (Optional) If yes circumvents parts of `apt-secure`, don't thread lightly
    #[deb822(field = "Allow-Insecure")]
    pub allow_insecure: Option<bool>, // TODO: redundant option, not present = default no
    /// (Optional) If yes circumvents parts of `apt-secure`, don't thread lightly
    #[deb822(field = "Allow-Weak")]
    pub allow_weak: Option<bool>, // TODO: redundant option, not present = default no
    /// (Optional) If yes circumvents parts of `apt-secure`, don't thread lightly
    #[deb822(field = "Allow-Downgrade-To-Insecure")]
    pub allow_downgrade_to_insecure: Option<bool>, // TODO: redundant option, not present = default no
    /// (Optional) If set forces whether APT considers source as rusted or no (default not present is a third state)
    #[deb822(field = "Trusted")]
    pub trusted: Option<bool>,
    /// (Optional) Contains either absolute path to GPG keyring or embedded GPG public key block, if not set APT uses all trusted keys;
    /// I can't find example of using with fingerprints
    #[deb822(field = "Signed-By")]
    pub signature: Option<Signature>,

    /// (Optional) Field ignored by APT but used by RepoLib to identify repositories, Ubuntu sources contain them
    #[deb822(field = "X-Repolib-Name")]
    pub x_repolib_name: Option<String>, // this supports RepoLib still used by PopOS, even if removed from Debian/Ubuntu

    /// (Optional) Field not present in the man page, but used in APT unit tests, potentially to hold the repository description
    #[deb822(field = "Description")]
    pub description: Option<String>, // options: HashMap<String, String> // My original parser kept remaining optional fields in the hash map, is this right approach?
}

impl Repository {
    /// Returns slice of strings containing suites for which this repository provides
    pub fn suites(&self) -> &[String] {
        self.suites.as_slice()
    }

    /// Generate legacy .list format lines for this repository
    pub fn to_legacy_format(&self) -> String {
        let mut lines = Vec::new();

        // Generate deb and/or deb-src lines
        for repo_type in &self.types {
            let type_str = match repo_type {
                RepositoryType::Binary => "deb",
                RepositoryType::Source => "deb-src",
            };

            for uri in &self.uris {
                for suite in &self.suites {
                    let mut line = format!("{} {} {}", type_str, uri, suite);

                    // Add components
                    if let Some(components) = &self.components {
                        for component in components {
                            line.push(' ');
                            line.push_str(component);
                        }
                    }

                    lines.push(line);
                }
            }
        }

        lines.join("\n") + "\n"
    }

    /// Parse a legacy apt sources.list line (e.g., "deb http://example.com/debian stable main")
    pub fn parse_legacy_line(line: &str) -> Result<Repository, String> {
        let parts: Vec<&str> = line.split_whitespace().collect();

        if parts.len() < 4 {
            return Err("Invalid repository line format".to_string());
        }

        let repo_type = match parts[0] {
            "deb" => RepositoryType::Binary,
            "deb-src" => RepositoryType::Source,
            _ => return Err("Line must start with 'deb' or 'deb-src'".to_string()),
        };

        let uri = Url::parse(parts[1]).map_err(|_| "Invalid URI".to_string())?;

        let suite = parts[2].to_string();
        let components: Vec<String> = parts[3..].iter().map(|s| s.to_string()).collect();

        Ok(Repository {
            enabled: Some(true),
            types: HashSet::from([repo_type]),
            uris: vec![uri],
            suites: vec![suite],
            components: Some(components),
            architectures: vec![],
            signature: None,
            ..Default::default()
        })
    }
}

/// Container for multiple `Repository` specifications as single `.sources` file may contain as per specification
#[derive(Debug)]
pub struct Repositories(Vec<Repository>);

impl Repositories {
    /// Creates empty container of repositories
    pub fn empty() -> Self {
        Repositories(Vec::new())
    }

    /// Creates repositories from container consisting `Repository` instances
    pub fn new<Container>(container: Container) -> Self
    where
        Container: Into<Vec<Repository>>,
    {
        Repositories(container.into())
    }

    /// Push a new repository
    pub fn push(&mut self, repo: Repository) {
        self.0.push(repo);
    }

    /// Retain repositories matching a predicate
    pub fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&Repository) -> bool,
    {
        self.0.retain(f);
    }

    /// Get mutable iterator over repositories
    pub fn iter_mut(&mut self) -> std::slice::IterMut<Repository> {
        self.0.iter_mut()
    }

    /// Extend with an iterator of repositories
    pub fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = Repository>,
    {
        self.0.extend(iter);
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl std::str::FromStr for Repositories {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let deb822: deb822_fast::Deb822 =
            s.parse().map_err(|e: deb822_fast::Error| e.to_string())?;

        let repos = deb822
            .iter()
            .map(Repository::from_paragraph)
            .collect::<Result<Vec<Repository>, Self::Err>>()?;
        Ok(Repositories(repos))
    }
}

impl std::fmt::Display for Repositories {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let result = self
            .0
            .iter()
            .map(|r| {
                let p: deb822_fast::Paragraph = r.to_paragraph();
                p.to_string()
            })
            .collect::<Vec<_>>()
            .join("\n");
        write!(f, "{}", result)
    }
}

impl Deref for Repositories {
    type Target = Vec<Repository>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashSet, str::FromStr};

    use indoc::indoc;
    use url::Url;

    use crate::{signature::Signature, Repositories, Repository, RepositoryType};

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
        assert_eq!(ret.unwrap_err(), "Unexpected token:  ".to_string());
    }

    #[test]
    fn test_parse_flat_repo() {
        let s = indoc! {r#"
            Types: deb
            URIs: http://ports.ubuntu.com/
            Suites: ./
            Architectures: arm64
        "#};

        let repos = s
            .parse::<Repositories>()
            .expect("Shall be parsed flawlessly");
        assert!(repos[0].types.contains(&super::RepositoryType::Binary));
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
        assert!(repos[0].types.contains(&super::RepositoryType::Binary));
        assert!(matches!(repos[0].signature, Some(Signature::KeyBlock(_))));
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

        let reps = s
            .parse::<Repositories>()
            .expect("Shall be parsed flawlessly");
        assert!(reps[0].types.contains(&super::RepositoryType::Binary));
        assert!(matches!(reps[0].signature, Some(Signature::KeyPath(_))));
    }

    #[test]
    fn test_serialize() {
        //let repos = Repositories::empty();
        let repos = Repositories::new([Repository {
            enabled: Some(true), // TODO: looks odd, as only `Enabled: no` in meaningful
            types: HashSet::from([RepositoryType::Binary]),
            architectures: vec!["arm64".to_owned()],
            uris: vec![Url::from_str("https://deb.debian.org/debian").unwrap()],
            suites: vec!["jammy".to_owned()],
            components: Some(vec!["main".to_owned()]),
            signature: None,
            x_repolib_name: None,
            languages: None,
            targets: None,
            pdiffs: None,
            ..Default::default()
        }]);
        let text = repos.to_string();
        assert_eq!(
            text,
            indoc! {r#"
            Enabled: yes
            Types: deb
            URIs: https://deb.debian.org/debian
            Suites: jammy
            Components: main
            Architectures: arm64
        "#}
        );
    }

    #[test]
    fn test_yesnoforce_to_string() {
        let yes = crate::YesNoForce::Yes;
        assert_eq!((&yes).to_string(), "yes");

        let no = crate::YesNoForce::No;
        assert_eq!((&no).to_string(), "no");

        let force = crate::YesNoForce::Force;
        assert_eq!((&force).to_string(), "force");
    }

    #[test]
    fn test_parse_legacy_line() {
        let line = "deb http://archive.ubuntu.com/ubuntu jammy main restricted";
        let repo = Repository::parse_legacy_line(line).unwrap();
        assert_eq!(repo.types.len(), 1);
        assert!(repo.types.contains(&RepositoryType::Binary));
        assert_eq!(repo.uris.len(), 1);
        assert_eq!(repo.uris[0].to_string(), "http://archive.ubuntu.com/ubuntu");
        assert_eq!(repo.suites, vec!["jammy".to_string()]);
        assert_eq!(
            repo.components,
            Some(vec!["main".to_string(), "restricted".to_string()])
        );
    }

    #[test]
    fn test_parse_legacy_line_invalid() {
        let line = "invalid line";
        let result = Repository::parse_legacy_line(line);
        assert!(result.is_err());
    }
}
