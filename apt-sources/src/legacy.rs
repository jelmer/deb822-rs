//! A library for parsing and manipulating APT source files that
//! use the DEB822 format to hold package repositories specifications.
//!
//! # Examples
use super::RepositoryError;
use super::RepositoryType;
use super::Signature;
use super::YesNoForce;
use itertools::Itertools;
use regex::Regex;
use std::collections::HashSet;
use std::ops::Deref;
use std::path::PathBuf;
use std::str::FromStr;
use url::Url;
use url_macro::url;

/// A structure representing APT repository as declared by one-line-style `.list` file
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
#[derive(Clone, PartialEq, /*Eq,*/ Debug)]
pub struct LegacyRepository {
    /// This doesn't represent real field, but rather commented or uncommented line
    enabled: bool,
    /// Legacy lists format support one type per line
    pub typ: RepositoryType,
    /// Single repo address; according to Debian that's URI, but this type is more advanced than URI from `http` crate
    pub uri: Url,
    /// The distribution name as codename or suite type (like `stable` or `testing`)
    pub suite: String,
    /// (Optional) Section of the repository, usually `main`, `contrib` or `non-free`
    /// return `None` if repository is Flat Repository Format (https://wiki.debian.org/DebianRepository/Format#Flat_Repository_Format)
    pub components: Vec<String>,

    /// (Optional) Architectures binaries from this repository run on
    pub architectures: Vec<String>, // arch
    /// (Optional) Translations support to download
    pub languages: Vec<String>, // lang
    /// (Optional) Download targets to acquire from this source
    pub targets: Vec<String>, // target
    /// (Optional) Controls if APT should try PDiffs instead of downloading indexes entirely; if not set defaults to configuration option `Acquire::PDiffs`
    pub pdiffs: Option<bool>, // pdiffs
    /// (Optional) Controls if APT should try to acquire indexes via a URI constructed from a hashsum of the expected file
    pub by_hash: Option<YesNoForce>, // by-hash
    /// (Optional) If yes circumvents parts of `apt-secure`, don't thread lightly
    pub allow_insecure: bool, // allow-insecure, default no
    /// (Optional) If yes circumvents parts of `apt-secure`, don't thread lightly
    pub allow_weak: bool, // allow-weak, default no
    /// (Optional) If yes circumvents parts of `apt-secure`, don't thread lightly
    pub allow_downgrade_to_insecure: bool, // allow-downgrade-to-insecure, default no
    /// (Optional) If set forces whether APT considers source as trusted or no (default not present is a third state)
    pub trusted: Option<bool>, // trusted
    /// (Optional) Contains either absolute path to GPG keyring or embedded GPG public key block, if not set APT uses all trusted keys;
    /// I can't find example of using with fingerprints
    pub signature: Option<Signature>, // signed-by

    /// (Optional) Field ignored by APT but used by RepoLib to identify repositories, Ubuntu sources contain them
    pub x_repolib_name: Option<String>, // this supports RepoLib still used by PopOS, even if removed from Debian/Ubuntu

    /// (Optional) Field not present in the man page, but used in APT unit tests, potentially to hold the repository description
    description: Option<String>, // options: HashMap<String, String> // [MF] My original parser kept remaining optional fields in the hash map, is this right approach?
}

impl Default for LegacyRepository {
    fn default() -> Self {
        Self {
            enabled: true,
            typ: RepositoryType::Binary,
            uri: url!("http://nowhere.com"),
            suite: "none".to_string(),
            components: vec![],
            architectures: vec![],
            languages: vec![],
            targets: vec![],
            pdiffs: None,
            by_hash: None,
            allow_insecure: false,
            allow_weak: false,
            allow_downgrade_to_insecure: false,
            trusted: None,
            signature: None,
            x_repolib_name: None,
            description: None,
        }
    }
}

impl LegacyRepository {
    /// In the ideal world we'd manage to use deserialization from a new format handling, but I'm not there yet to lift this for this format
    fn assign_option_field(&mut self, key: &str, value: &str) -> Result<(), RepositoryError> {
        match key {
            "arch" => self.architectures = value.split(',').map(|s| s.to_string()).collect(),
            "lang" => self.languages = value.split(',').map(|s| s.to_string()).collect(),
            "target" => self.targets = value.split(',').map(|s| s.to_string()).collect(),
            "pdiffs" => self.pdiffs = Some(super::deserialize_yesno(value)?),
            "by-hash" => self.by_hash = Some(YesNoForce::from_str(value)?),
            "allow-insecure" => self.allow_insecure = super::deserialize_yesno(value)?, // , default no
            "allow-weak" => self.allow_weak = super::deserialize_yesno(value)?, // , default no
            "allow-downgrade-to-insecure" => self.allow_weak = super::deserialize_yesno(value)?, // , default no
            "trusted" => self.trusted = Some(super::deserialize_yesno(value)?), // default not present is a third state
            "signed-by" => {
                self.signature = Some(Signature::KeyPath(PathBuf::from_str(value).unwrap()))
                // TODO: PathBuf::from_str() has `Infallible` as `Err` type
            }
            "x_repo_lib_name" => self.x_repolib_name = Some(value.to_string()),
            "description" => self.description = Some(value.to_string()),
            _ => return Err(RepositoryError::UnrecognizedFieldName),
        };
        Ok(())
    }
}

/// Container for multiple `LegacyRepository` specifications as single `.list` file may contain as per specification
#[derive(Debug)]
pub struct LegacyRepositories(Vec<LegacyRepository>);

impl LegacyRepositories {
    /// Creates empty container of repositories
    pub fn empty() -> Self {
        Self(Vec::new())
    }

    /// Creates repositories from container consisting `Repository` instances
    pub fn new<Container>(container: Container) -> Self
    where
        Container: Into<Vec<LegacyRepository>>,
    {
        Self(container.into())
    }

    /// Provides iterator over individual repositories in the whole file
    pub fn repositories(&self) -> impl Iterator<Item = &LegacyRepository> {
        // TODO: that's by ref, not compatible with lossless
        self.0.iter()
    }

    /// Push a new repository
    pub fn push(&mut self, repo: LegacyRepository) {
        self.0.push(repo);
    }

    /// Retain repositories matching a predicate
    pub fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&LegacyRepository) -> bool,
    {
        self.0.retain(f);
    }

    /// Get mutable iterator over repositories
    pub fn iter_mut(&mut self) -> std::slice::IterMut<LegacyRepository> {
        self.0.iter_mut()
    }

    /// Extend with an iterator of repositories
    pub fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = LegacyRepository>,
    {
        self.0.extend(iter);
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

/// It only make sense to convert multiple lines at once as typical `.list` file has one uncommented
/// and one commented (`deb-src`) entry
impl FromStr for LegacyRepositories {
    type Err = RepositoryError;

    fn from_str(text: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(
            r"(?x)^
            (?P<type>deb|deb-src)\s+                   # Catch repository type
            (?P<options>\[[^]]*])?\s+                  # Catch options
            (?P<uri>\S+)\s+                            # Catch repository URI
            (?P<suite>\S+)\s+                          # Catch suite/distribution
            (?P<components>(?:(?P<component>\w+)\s?)+) # Catch components (multiple)
            $",
        )
        .unwrap(); // TODO: Make this lazy static for efficient reuse.
        let elements = re
            .captures_iter(text)
            .map(|caps| {
                let mut repository = LegacyRepository::default();
                repository.typ = RepositoryType::from_str(&caps["type"])?;
                // repository.options = caps["options"]
                //     .trim_matches(|c| c == '[' || c == ']')
                //     .split_whitespace()
                //     .map(|o| o.splitn(2, '=').collect_tuple::<(&str, &str)>().unwrap())
                //     .map(|(k, v)| (k.to_deb822_option(), v.to_owned()))
                //     .collect();
                caps["options"]
                    .trim_matches(|c| c == '[' || c == ']')
                    .split_whitespace()
                    .map(|o| o.splitn(2, '=').collect_tuple::<(&str, &str)>().unwrap()) // TODO: can we do something about `unwrap()`?
                    //.map(|(k, v)| (k.to_deb822_option(), v)) // TODO: we don't need this
                    .try_for_each(|(k, v)| repository.assign_option_field(k, v))?;
                repository.uri = Url::from_str(&caps["uri"])?;
                repository.suite = caps["suite"].to_owned();
                repository
                    .components
                    .extend(caps["components"].split_whitespace().map(|c| c.to_owned()));
                <Result<LegacyRepository, Self::Err>>::Ok(repository)
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self(elements))
    }
}

impl Deref for LegacyRepositories {
    type Target = Vec<LegacyRepository>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[test]
    fn test_legacy_repositories_from_str() {
        const SAMPLE: &str = indoc!("
            deb [arch=arm64 signed-by=/usr/share/keyrings/rcn-ee-archive-keyring.gpg] http://debian.beagleboard.org/arm64/ jammy main
        ");

        let repositories =
            LegacyRepositories::from_str(SAMPLE).expect("Shall not fail for correct list entry!");

        assert_eq!(repositories.len(), 1);

        let repository = repositories.iter().nth(0).unwrap();

        assert_eq!(repository.enabled, true);
        assert_eq!(repository.typ, RepositoryType::Binary);
        assert_eq!(repository.uri, url!("http://debian.beagleboard.org/arm64/"));
        assert_eq!(repository.suite, "jammy".to_owned());
        assert_eq!(repository.components, vec!["main".to_owned()]);
        assert_eq!(repository.x_repolib_name, None);
        // assert_eq!(
        //     repository.options,
        //     HashMap::from([
        //         (
        //             "Signed-By".to_owned(),
        //             "/usr/share/keyrings/rcn-ee-archive-keyring.gpg".to_owned()
        //         ),
        //         ("Architectures".to_owned(), "arm64".to_owned())
        //     ])
        // );
        // assert_eq!(repository.format, RepositoryFormat::List);
    }
}
