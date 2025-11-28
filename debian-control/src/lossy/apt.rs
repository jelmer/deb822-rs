//! APT related structures
use crate::lossy::Relations;
use deb822_fast::{FromDeb822, FromDeb822Paragraph, ToDeb822, ToDeb822Paragraph};

fn deserialize_yesno(s: &str) -> Result<bool, String> {
    match s {
        "yes" => Ok(true),
        "no" => Ok(false),
        _ => Err(format!("invalid value for yesno: {}", s)),
    }
}

fn serialize_yesno(b: &bool) -> String {
    if *b {
        "yes".to_string()
    } else {
        "no".to_string()
    }
}

fn deserialize_components(value: &str) -> Result<Vec<String>, String> {
    Ok(value.split_whitespace().map(|s| s.to_string()).collect())
}

fn join_whitespace(components: &[String]) -> String {
    components.join(" ")
}

fn deserialize_architectures(value: &str) -> Result<Vec<String>, String> {
    Ok(value.split_whitespace().map(|s| s.to_string()).collect())
}

#[derive(Debug, Clone, PartialEq, Eq, ToDeb822, FromDeb822)]
/// A Release file
pub struct Release {
    #[deb822(field = "Codename")]
    /// The codename of the release
    pub codename: String,

    #[deb822(
        field = "Components",
        deserialize_with = deserialize_components,
        serialize_with = join_whitespace
    )]
    /// Components supported by the release
    pub components: Vec<String>,

    #[deb822(
        field = "Architectures",
        deserialize_with = deserialize_architectures,
        serialize_with = join_whitespace
    )]
    /// Architectures supported by the release
    pub architectures: Vec<String>,

    #[deb822(field = "Description")]
    /// Description of the release
    pub description: String,

    #[deb822(field = "Origin")]
    /// Origin of the release
    pub origin: String,

    #[deb822(field = "Label")]
    /// Label of the release
    pub label: String,

    #[deb822(field = "Suite")]
    /// Suite of the release
    pub suite: String,

    #[deb822(field = "Version")]
    /// Version of the release
    pub version: String,

    #[deb822(field = "Date")]
    /// Date the release was published
    pub date: String,

    #[deb822(field = "NotAutomatic", deserialize_with = deserialize_yesno, serialize_with = serialize_yesno)]
    /// Whether the release is not automatic
    pub not_automatic: bool,

    #[deb822(field = "ButAutomaticUpgrades", deserialize_with = deserialize_yesno, serialize_with = serialize_yesno)]
    /// Indicates if packages retrieved from this release should be automatically upgraded
    pub but_automatic_upgrades: bool,

    #[deb822(field = "Acquire-By-Hash", deserialize_with = deserialize_yesno, serialize_with = serialize_yesno)]
    /// Whether packages files can be acquired by hash
    pub acquire_by_hash: bool,
}

fn deserialize_binaries(value: &str) -> Result<Vec<String>, String> {
    Ok(value.split_whitespace().map(|s| s.to_string()).collect())
}

fn join_lines(components: &[String]) -> String {
    components.join("\n")
}

fn deserialize_package_list(value: &str) -> Result<Vec<String>, String> {
    Ok(value.split('\n').map(|s| s.to_string()).collect())
}

#[derive(Debug, Clone, PartialEq, Eq, ToDeb822, FromDeb822)]
/// A source
pub struct Source {
    #[deb822(field = "Directory")]
    /// The directory of the source
    pub directory: String,

    #[deb822(field = "Description")]
    /// Description of the source
    pub description: Option<String>,

    #[deb822(field = "Version")]
    /// Version of the source
    pub version: debversion::Version,

    #[deb822(field = "Package")]
    /// Package of the source
    pub package: String,

    #[deb822(field = "Binary", deserialize_with = deserialize_binaries, serialize_with = join_whitespace)]
    /// Binaries of the source
    pub binaries: Option<Vec<String>>,

    #[deb822(field = "Maintainer")]
    /// Maintainer of the source
    pub maintainer: Option<String>,

    #[deb822(field = "Build-Depends")]
    /// Build dependencies of the source
    pub build_depends: Option<String>,

    #[deb822(field = "Build-Depends-Indep")]
    /// Build dependencies independent of the architecture of the source
    pub build_depends_indep: Option<Relations>,

    #[deb822(field = "Build-Conflicts")]
    /// Build conflicts of the source
    pub build_conflicts: Option<Relations>,

    #[deb822(field = "Build-Conflicts-Indep")]
    /// Build conflicts independent of the architecture of the source
    pub build_conflicts_indep: Option<Relations>,

    #[deb822(field = "Standards-Version")]
    /// Standards version of the source
    pub standards_version: Option<String>,

    #[deb822(field = "Homepage")]
    /// Homepage of the source
    pub homepage: Option<String>,

    #[deb822(field = "Autobuild")]
    /// Whether the source should be autobuilt
    pub autobuild: Option<bool>,

    #[deb822(field = "Testsuite")]
    /// Testsuite of the source
    pub testsuite: Option<String>,

    #[deb822(field = "Vcs-Browser")]
    /// VCS browser of the source
    pub vcs_browser: Option<String>,

    #[deb822(field = "Vcs-Git")]
    /// VCS Git of the source
    pub vcs_git: Option<String>,

    #[deb822(field = "Vcs-Bzr")]
    /// VCS Bzr of the source
    pub vcs_bzr: Option<String>,

    #[deb822(field = "Vcs-Hg")]
    /// VCS Hg of the source
    pub vcs_hg: Option<String>,

    #[deb822(field = "Vcs-Svn")]
    /// VCS SVN of the source
    pub vcs_svn: Option<String>,

    #[deb822(field = "Vcs-Darcs")]
    /// VCS Darcs of the source
    pub vcs_darcs: Option<String>,

    #[deb822(field = "Vcs-Cvs")]
    /// VCS CVS of the source
    pub vcs_cvs: Option<String>,

    #[deb822(field = "Vcs-Arch")]
    /// VCS Arch of the source
    pub vcs_arch: Option<String>,

    #[deb822(field = "Vcs-Mtn")]
    /// VCS Mtn of the source
    pub vcs_mtn: Option<String>,

    #[deb822(field = "Priority")]
    /// Priority of the source
    pub priority: Option<crate::fields::Priority>,

    #[deb822(field = "Section")]
    /// Section of the source
    pub section: Option<String>,

    #[deb822(field = "Format")]
    /// Format of the source
    pub format: Option<String>,

    #[deb822(field = "Package-List", deserialize_with = deserialize_package_list, serialize_with = join_lines)]
    /// Package list of the source
    pub package_list: Vec<String>,
}

impl std::str::FromStr for Source {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let para = s
            .parse::<deb822_fast::Paragraph>()
            .map_err(|e| e.to_string())?;

        FromDeb822Paragraph::from_paragraph(&para)
    }
}

impl std::fmt::Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let para: deb822_fast::Paragraph = self.to_paragraph();
        write!(f, "{}", para)
    }
}

/// A package
#[derive(Debug, Clone, PartialEq, Eq, ToDeb822, FromDeb822)]
pub struct Package {
    /// The name of the package
    #[deb822(field = "Package")]
    pub name: String,

    /// The version of the package
    #[deb822(field = "Version")]
    pub version: debversion::Version,

    /// The architecture of the package
    #[deb822(field = "Architecture")]
    pub architecture: String,

    /// The maintainer of the package
    #[deb822(field = "Maintainer")]
    pub maintainer: Option<String>,

    /// The installed size of the package
    #[deb822(field = "Installed-Size")]
    pub installed_size: Option<usize>,

    /// Dependencies
    #[deb822(field = "Depends")]
    pub depends: Option<Relations>,

    /// Pre-Depends
    #[deb822(field = "Pre-Depends")]
    pub pre_depends: Option<Relations>,

    /// Recommends
    #[deb822(field = "Recommends")]
    pub recommends: Option<Relations>,

    /// Suggests
    #[deb822(field = "Suggests")]
    pub suggests: Option<Relations>,

    /// Enhances
    #[deb822(field = "Enhances")]
    pub enhances: Option<Relations>,

    /// Breaks
    #[deb822(field = "Breaks")]
    pub breaks: Option<Relations>,

    /// Conflicts
    #[deb822(field = "Conflicts")]
    pub conflicts: Option<Relations>,

    /// Provides
    #[deb822(field = "Provides")]
    pub provides: Option<Relations>,

    /// Replaces
    #[deb822(field = "Replaces")]
    pub replaces: Option<Relations>,

    /// Built-Using
    #[deb822(field = "Built-Using")]
    pub built_using: Option<Relations>,

    /// Description
    #[deb822(field = "Description")]
    pub description: Option<String>,

    /// Homepage
    #[deb822(field = "Homepage")]
    pub homepage: Option<String>,

    /// Priority
    #[deb822(field = "Priority")]
    pub priority: Option<crate::fields::Priority>,

    /// Section
    #[deb822(field = "Section")]
    pub section: Option<String>,

    /// Essential
    #[deb822(field = "Essential", deserialize_with = deserialize_yesno, serialize_with = serialize_yesno)]
    pub essential: Option<bool>,

    /// Tag
    #[deb822(field = "Tag")]
    pub tag: Option<String>,

    /// Size
    #[deb822(field = "Size")]
    pub size: Option<usize>,

    /// MD5sum
    #[deb822(field = "MD5sum")]
    pub md5sum: Option<String>,

    /// SHA256
    #[deb822(field = "SHA256")]
    pub sha256: Option<String>,

    /// Description (MD5)
    #[deb822(field = "Description-MD5")]
    pub description_md5: Option<String>,
}

impl std::str::FromStr for Package {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let para = s
            .parse::<deb822_fast::Paragraph>()
            .map_err(|e| e.to_string())?;

        FromDeb822Paragraph::from_paragraph(&para)
    }
}

impl std::fmt::Display for Package {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let para: deb822_fast::Paragraph = self.to_paragraph();
        write!(f, "{}", para)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use deb822_fast::Paragraph;
    use deb822_fast::ToDeb822Paragraph;

    #[test]
    fn test_release() {
        let release = Release {
            codename: "focal".to_string(),
            components: vec!["main".to_string(), "restricted".to_string()],
            architectures: vec!["amd64".to_string(), "arm64".to_string()],
            description: "Ubuntu 20.04 LTS".to_string(),
            origin: "Ubuntu".to_string(),
            label: "Ubuntu".to_string(),
            suite: "focal".to_string(),
            version: "20.04".to_string(),
            date: "Thu, 23 Apr 2020 17:19:19 UTC".to_string(),
            not_automatic: false,
            but_automatic_upgrades: true,
            acquire_by_hash: true,
        };

        let deb822 = r#"Codename: focal
Components: main restricted
Architectures: amd64 arm64
Description: Ubuntu 20.04 LTS
Origin: Ubuntu
Label: Ubuntu
Suite: focal
Version: 20.04
Date: Thu, 23 Apr 2020 17:19:19 UTC
NotAutomatic: no
ButAutomaticUpgrades: yes
Acquire-By-Hash: yes
"#;

        let para = deb822.parse::<Paragraph>().unwrap();

        let release: deb822_fast::Paragraph = release.to_paragraph();

        assert_eq!(release, para);
    }

    #[test]
    fn test_package() {
        let package = r#"Package: apt
Version: 2.1.10
Architecture: amd64
Maintainer: APT Development Team <apt@lists.debian.org>
Installed-Size: 3524
Depends: libc6 (>= 2.14), libgcc1
Pre-Depends: dpkg (>= 1.15.6)
Recommends: gnupg
Suggests: apt-doc, aptitude | synaptic | wajig
"#;

        let package: Package = package.parse().unwrap();

        assert_eq!(package.name, "apt");
        assert_eq!(package.version, "2.1.10".parse().unwrap());
        assert_eq!(package.architecture, "amd64");
    }

    #[test]
    fn test_package_essential() {
        let package = r#"Package: base-files
Version: 11.1
Architecture: amd64
Essential: yes
"#;

        let package: Package = package.parse().unwrap();

        assert_eq!(package.name, "base-files");
        assert_eq!(package.essential, Some(true));
    }

    #[test]
    fn test_package_essential_no() {
        let package = r#"Package: apt
Version: 2.1.10
Architecture: amd64
Essential: no
"#;

        let package: Package = package.parse().unwrap();

        assert_eq!(package.name, "apt");
        assert_eq!(package.essential, Some(false));
    }
}
