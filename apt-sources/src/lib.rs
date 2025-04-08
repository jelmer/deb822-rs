#![deny(missing_docs)]
//! A library for parsing and manipulating APT source files that
//! use the DEB822 format to hold package repositories specifications.
//!
//! # Examples
//!
//! ```rust
//! use apt_sources::{Repositories, traits::Repository};
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
//! See the ``lossless`` module (behind the ``lossless`` feature) for a more forgiving parser that
//! allows partial parsing, parsing files with errors and unknown fields and editing while
//! preserving formatting.

use deb822_lossless::{FromDeb822, FromDeb822Paragraph, ToDeb822, ToDeb822Paragraph};
use error::RepositoryError;
use signature::Signature;
use std::result::Result;
use std::{borrow::Cow, collections::HashSet, fmt::Display, ops::Deref, str::FromStr};
use url::Url;

pub mod error;
#[cfg(feature = "lossless")]
pub mod lossless;
pub mod signature;
pub mod traits;

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

impl ToString for RepositoryType {
    fn to_string(&self) -> String {
        self.into()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

// impl From<&YesNoForce> for String {
//     fn from(value: &YesNoForce) -> Self {
//         match value {
//             YesNoForce::Yes => "yes".to_owned(),
//             YesNoForce::No => "no".to_owned(),
//             YesNoForce::Force => "force".to_owned()
//         }
//     }
// }

impl Display for YesNoForce {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            YesNoForce::Yes => write!(f, "yes"),
            YesNoForce::No => write!(f, "no"),
            YesNoForce::Force => write!(f, "force"),
        }
    }
}

fn deserialize_types(text: &str) -> Result<HashSet<RepositoryType>, RepositoryError> {
    text.split_whitespace()
        .map(|t| RepositoryType::from_str(t))
        .collect::<Result<HashSet<RepositoryType>, RepositoryError>>()
}

fn serialize_types(files: &HashSet<RepositoryType>) -> String {
    files
        .into_iter()
        .map(|rt| rt.to_string())
        .collect::<Vec<String>>()
        .join("\n")
}

fn deserialize_uris(text: &str) -> Result<Vec<Url>, String> {
    // TODO: bad error type
    text.split_whitespace()
        .map(|u| Url::from_str(u))
        .collect::<Result<Vec<Url>, _>>()
        .map_err(|e| e.to_string()) // TODO: bad error type
}

fn serialize_uris(uris: &[Url]) -> String {
    uris.into_iter()
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
    enabled: Option<bool>,

    /// The value `RepositoryType::Binary` (`deb`) or/and `RepositoryType::Source` (`deb-src`)
    #[deb822(field = "Types", deserialize_with = deserialize_types, serialize_with = serialize_types)]
    types: HashSet<RepositoryType>, // consider alternative, closed set
    /// The address of the repository
    #[deb822(field = "URIs", deserialize_with = deserialize_uris, serialize_with = serialize_uris)]
    uris: Vec<Url>, // according to Debian that's URI, but this type is more advanced than URI from `http` crate
    /// The distribution name as codename or suite type (like `stable` or `testing`)
    #[deb822(field = "Suites", deserialize_with = deserialize_string_chain, serialize_with = serialize_string_chain)]
    suites: Vec<String>,
    /// (Optional) Section of the repository, usually `main`, `contrib` or `non-free`
    /// return `None` if repository is Flat Repository Format (https://wiki.debian.org/DebianRepository/Format#Flat_Repository_Format)
    #[deb822(field = "Components", deserialize_with = deserialize_string_chain, serialize_with = serialize_string_chain)]
    components: Option<Vec<String>>,

    /// (Optional) Architectures binaries from this repository run on
    #[deb822(field = "Architectures", deserialize_with = deserialize_string_chain, serialize_with = serialize_string_chain)]
    architectures: Option<Vec<String>>,
    /// (Optional) Translations support to download
    #[deb822(field = "Languages", deserialize_with = deserialize_string_chain, serialize_with = serialize_string_chain)]
    languages: Option<Vec<String>>, // TODO: Option is redundant to empty vectors
    /// (Optional) Download targets to acquire from this source
    #[deb822(field = "Targets", deserialize_with = deserialize_string_chain, serialize_with = serialize_string_chain)]
    targets: Option<Vec<String>>,
    /// (Optional) Controls if APT should try PDiffs instead of downloading indexes entirely; if not set defaults to configuration option `Acquire::PDiffs`
    #[deb822(field = "PDiffs", deserialize_with = deserialize_yesno)]
    pdiffs: Option<bool>,
    /// (Optional) Controls if APT should try to acquire indexes via a URI constructed from a hashsum of the expected file
    #[deb822(field = "By-Hash")]
    by_hash: Option<YesNoForce>,
    /// (Optional) If yes circumvents parts of `apt-secure`, don't thread lightly
    #[deb822(field = "Allow-Insecure")]
    allow_insecure: Option<bool>, // TODO: redundant option, not present = default no
    /// (Optional) If yes circumvents parts of `apt-secure`, don't thread lightly
    #[deb822(field = "Allow-Weak")]
    allow_weak: Option<bool>, // TODO: redundant option, not present = default no
    /// (Optional) If yes circumvents parts of `apt-secure`, don't thread lightly
    #[deb822(field = "Allow-Downgrade-To-Insecure")]
    allow_downgrade_to_insecure: Option<bool>, // TODO: redundant option, not present = default no
    /// (Optional) If set forces whether APT considers source as rusted or no (default not present is a third state)
    #[deb822(field = "Trusted")]
    trusted: Option<bool>,
    /// (Optional) Contains either absolute path to GPG keyring or embedded GPG public key block, if not set APT uses all trusted keys;
    /// I can't find example of using with fingerprints
    #[deb822(field = "Signed-By")]
    signature: Option<Signature>,

    /// (Optional) Field ignored by APT but used by RepoLib to identify repositories, Ubuntu sources contain them
    #[deb822(field = "X-Repolib-Name")]
    x_repolib_name: Option<String>, // this supports RepoLib still used by PopOS, even if removed from Debian/Ubuntu

    /// (Optional) Field not present in the man page, but used in APT unit tests, potentially to hold the repository description
    #[deb822(field = "Description")]
    description: Option<String>, // options: HashMap<String, String> // [MF] My original parser kept remaining optional fields in the hash map, is this right approach?
}

impl Repository {
    /// Creates empty instance of `Repository` which is a single entry for `Repositories`` (a _Paragraph_ in DEB822 sense)
    pub fn empty() -> Self {
        Self {
            enabled: None,
            types: HashSet::with_capacity(1), // [MF] Assuming some would like to set it to `Binary`
            uris: vec![],
            suites: vec![],
            components: None,
            architectures: None,
            languages: None,
            targets: None,
            pdiffs: None,
            by_hash: None,
            allow_insecure: None,
            allow_weak: None,
            allow_downgrade_to_insecure: None,
            trusted: None,
            signature: None,
            x_repolib_name: None,
            description: None,
        }
    }
}

impl traits::Repository for Repository {
    fn suites(&self) -> Cow<'_, [String]> {
        Cow::Borrowed(self.suites.as_slice())
    }

    fn enabled(&self) -> bool {
        self.enabled.is_none_or(|x| x)
    }

    fn types(&self) -> Cow<'_, HashSet<RepositoryType>> {
        Cow::Borrowed(&self.types)
    }

    fn uris(&self) -> Cow<'_, [url::Url]> {
        Cow::Borrowed(self.uris.as_slice())
    }

    fn components(&self) -> Cow<'_, [String]> {
        Cow::Borrowed(self.components.as_ref().map_or(&[], |x| x.as_slice()))
    }

    fn architectures(&self) -> Cow<'_, [String]> {
        Cow::Borrowed(self.architectures.as_ref().map_or(&[], |x| x.as_slice()))
    }

    fn languages(&self) -> Cow<'_, [String]> {
        Cow::Borrowed(self.languages.as_ref().map_or(&[], |x| x.as_slice()))
    }

    fn targets(&self) -> Cow<'_, [String]> {
        Cow::Borrowed(self.targets.as_ref().map_or(&[], |x| x.as_slice()))
    }

    fn pdiffs(&self) -> Option<bool> {
        self.pdiffs
    }

    fn by_hash(&self) -> Option<YesNoForce> {
        self.by_hash
    }

    fn allow_insecure(&self) -> Option<bool> {
        self.allow_insecure
    }

    fn allow_weak(&self) -> Option<bool> {
        self.allow_weak
    }

    fn allow_downgrade_to_insecure(&self) -> Option<bool> {
        self.allow_downgrade_to_insecure
    }

    fn trusted(&self) -> Option<bool> {
        self.trusted
    }

    fn signature(&self) -> Option<Cow<'_, Signature>> {
        self.signature.as_ref().map(|v| Cow::Borrowed(v))
    }

    fn x_repolib_name(&self) -> Option<Cow<'_, str>> {
        self.x_repolib_name
            .as_ref()
            .map(|v| Cow::Borrowed(v.as_str()))
    }

    fn description(&self) -> Option<Cow<'_, str>> {
        self.description.as_ref().map(|v| Cow::Borrowed(v.as_str()))
    }
}

impl traits::RepositoryMut for Repository {
    fn set_enabled(&mut self, enabled: bool) {
        self.enabled = match enabled {
            true => None,
            false => Some(false),
        }
    }

    fn set_types<T>(&mut self, types: T)
    where
        T: IntoIterator<Item = RepositoryType>,
    {
        self.types = types.into_iter().collect();
    }

    fn set_uris(&mut self, uris: &[Url]) {
        self.uris = uris.into_iter().map(|u| u.to_owned()).collect();
    }

    fn set_suites<T, I>(&mut self, suites: T)
    where
        T: IntoIterator<Item = I>,
        I: AsRef<str>,
    {
        self.suites = suites.into_iter().map(|s| s.as_ref().to_owned()).collect();
    }

    fn set_components<T, A, I>(&mut self, components: T)
    where
        T: IntoIterator<Item = A, IntoIter = I>,
        I: ExactSizeIterator<Item = A>,
        A: AsRef<str>,
    {
        let iter = components.into_iter();
        if iter.len() != 0 {
            self.components = Some(iter.map(|s| s.as_ref().to_owned()).collect());
        } else {
            self.components = None;
        }
    }

    fn set_architectures<T, A, I>(&mut self, architectures: T)
    where
        T: IntoIterator<Item = A, IntoIter = I>,
        I: ExactSizeIterator<Item = A>,
        A: AsRef<str>,
    {
        let iter = architectures.into_iter();
        if iter.len() != 0 {
            self.architectures = Some(iter.map(|s| s.as_ref().to_owned()).collect());
        } else {
            self.architectures = None;
        }
    }

    fn set_languages<T, L, I>(&mut self, languages: T)
    where
        T: IntoIterator<Item = L, IntoIter = I>,
        I: ExactSizeIterator<Item = L>,
        L: AsRef<str>,
    {
        let iter = languages.into_iter();
        if iter.len() != 0 {
            self.languages = Some(iter.map(|s| s.as_ref().to_owned()).collect());
        } else {
            self.languages = None;
        }
    }

    fn set_targets<T, G, I>(&mut self, targets: T)
    where
        T: IntoIterator<Item = G, IntoIter = I>,
        I: ExactSizeIterator<Item = G>,
        G: AsRef<str>,
    {
        let iter = targets.into_iter();
        self.targets = if iter.len() != 0 {
            Some(iter.map(|s| s.as_ref().to_owned()).collect())
        } else {
            None
        }
    }

    fn set_pdiffs(&mut self, pdiffs: Option<bool>) {
        self.pdiffs = pdiffs;
    }

    fn set_by_hash(&mut self, by_hash: Option<YesNoForce>) {
        self.by_hash = by_hash;
    }

    fn set_allow_insecure(&mut self, allow_insecure: Option<bool>) {
        self.allow_insecure = allow_insecure;
    }

    fn set_allow_weak(&mut self, allow_weak: Option<bool>) {
        self.allow_weak = allow_weak;
    }

    fn set_allow_downgrade_to_insecure(&mut self, adti: Option<bool>) {
        self.allow_downgrade_to_insecure = adti;
    }

    fn set_trusted(&mut self, trusted: Option<bool>) {
        self.trusted = trusted;
    }

    fn set_signature<S, R>(&mut self, signature: S)
    where
        S: AsRef<Option<R>>,
        R: AsRef<Signature>
    {
        self.signature = match signature.as_ref() {
            Some(x) => Some(x.as_ref().to_owned()),
            None => None
        }
        //self.signature = signature.as_ref().to_owned();
    }

    fn set_x_repolib_name<N>(&mut self, x_repolib_name: N)
    where
        N: AsRef<str>,
    {
        if x_repolib_name.as_ref().len() != 0 {
            self.x_repolib_name = Some(x_repolib_name.as_ref().to_owned());
        } else {
            self.x_repolib_name = None;
        }
    }

    fn set_description<D>(&mut self, description: D)
    where
        D: AsRef<str>,
    {
        if description.as_ref().len() != 0 {
            self.description = Some(description.as_ref().to_owned());
        } else {
            self.description = None;
        }
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

    /// Provides iterator over individual repositories in the whole file
    pub fn repositories(&self) -> impl Iterator<Item = &Repository> {
        // TODO: that's by ref, not compatible with lossless
        self.0.iter()
    }
}

impl std::str::FromStr for Repositories {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let deb822: deb822_lossless::Deb822 = s
            .parse()
            .map_err(|e: deb822_lossless::ParseError| e.to_string())?;

        let repos = deb822
            .paragraphs()
            .map(|p| Repository::from_paragraph(&p))
            .collect::<Result<Vec<Repository>, Self::Err>>()?;
        Ok(Repositories(repos))
    }
}

impl ToString for Repositories {
    fn to_string(&self) -> String {
        self.0
            .iter()
            .map(|r| {
                let p: deb822_lossless::lossy::Paragraph = r.to_paragraph();
                p.to_string()
            })
            .collect::<Vec<_>>()
            .join("\n")
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
    use std::path::PathBuf;
    use std::{collections::HashSet, str::FromStr};

    use indoc::indoc;
    use url::Url;

    use crate::{signature::Signature, Repositories, Repository, RepositoryType};

    use crate::traits::{Repository as RepositoryTrait, RepositoryMut};

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
        assert!(repos[0].types.contains(&super::RepositoryType::Binary));
        assert_eq!(
            repos[0].components().as_ref(),
            [
                "main".to_owned(),
                "restricted".to_owned(),
                "universe".to_owned(),
                "multiverse".to_owned()
            ]
        );
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
            architectures: Some(vec!["arm64".to_owned()]),
            uris: vec![Url::from_str("https://deb.debian.org/debian").unwrap()],
            suites: vec!["jammy".to_owned()],
            components: vec!["main".to_owned()].into(),
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
    fn test_mutable_trait_then_serialize() {
        let mut r = Repository::empty();

        r.set_types([RepositoryType::Binary]);
        r.set_uris(&[Url::parse("https://packages.distro.rs/debian").expect("Right URL")]);
        r.set_suites(vec!["stable"]);
        r.set_components(vec!["main".to_owned()]);
        r.set_architectures(["arm64"]);
        let s = Signature::KeyPath(PathBuf::from("/usr/share/keyrings/jelmer.gpg"));
        //r.set_signature(Some(&s)); // TODO: this stuff doesn't build yet

        let r = Repositories::new([r]);

        let text = r.to_string();

        assert_eq!(
            text,
            indoc! {r#"
                Types: deb
                URIs: https://packages.distro.rs/debian
                Suites: stable
                Components: main
                Architectures: arm64
            "#});
    }
}
