//! Module holds traits for dealing with two kinds of repository sources representation in convenient polymorphic way
use std::{borrow::Cow, collections::HashSet};

use url::Url;

use crate::{signature::Signature, RepositoryType, YesNoForce};

/// This trait commonize immutable access to both lossy and lossless representation of a APT's source repository
pub trait Repository {
    /// Whether the repository source is active and taken into account by APT
    fn enabled(&self) -> bool;

    /// The value `RepositoryType ->  -> Binary` (`deb`) or/and `RepositoryType ->  -> Source` (`deb-src`)
    fn types(&self) -> HashSet<RepositoryType>;

    /// The address(es) of the repository
    fn uris(&self) -> Cow<'_, [Url]>;

    /// The distribution name as codename or suite type (like `stable` or `testing`)
    fn suites(&self) -> Cow<'_, [String]>;

    /// Section of the repository, usually `main`, `contrib` or `non-free`
    fn components(&self) -> Cow<'_, [String]>;

    /// (Optional) Architectures binaries from this repository run on
    fn architectures(&self) -> Cow<'_, [String]>;
    
    /// (Optional) Translations support to download
    fn languages(&self) -> Cow<'_, [String]>;

    /// (Optional) Download targets to acquire from this source
    fn targets(&self) ->  Cow<'_, [String]>;
    
    /// (Optional) Controls if APT should try PDiffs instead of downloading indexes entirely; if not set defaults to configuration option `Acquire ->  -> PDiffs`
    fn pdiffs(&self) -> Option<bool>;

    /// (Optional) Controls if APT should try to acquire indexes via a URI constructed from a hashsum of the expected file
    fn by_hash(&self) -> Option<YesNoForce>;

    /// (Optional) If yes circumvents parts of `apt-secure`, don't thread lightly
    fn allow_insecure(&self) -> Option<bool>;

    /// (Optional) If yes circumvents parts of `apt-secure`, don't thread lightly
    fn allow_weak(&self) -> Option<bool>;

    /// (Optional) If yes circumvents parts of `apt-secure`, don't thread lightly
    fn allow_downgrade_to_insecure(&self) -> Option<bool>; // TODO: redundant option, not present = default no

    /// (Optional) If set forces whether APT considers source as rusted or no (default not present is a third state)
    fn trusted(&self) -> Option<bool>;

    /// (Optional) Contains either absolute path to GPG keyring or embedded GPG public key block, if not set APT uses all trusted keys;
    /// I can't find example of using with fingerprints
    fn signature(&self) -> Option<Cow<'_, Signature>>;
    /// alias signed_by

    /// (Optional) Field ignored by APT but used by RepoLib to identify repositories, Ubuntu sources contain them
    fn x_repolib_name(&self) -> Option<Cow<'_, str>>; // this supports RepoLib still used by PopOS, even if removed from Debian/Ubuntu

    /// (Optional) Field not present in the man page, but used in APT unit tests, potentially to hold the repository description
    fn description(&self) -> Option<Cow<'_, str>>;
}
