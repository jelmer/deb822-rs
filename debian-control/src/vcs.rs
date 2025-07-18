//! Version Control System information
use regex::Regex;
use std::str::FromStr;

/// Parsed VCS information
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedVcs {
    /// URL of the repository
    pub repo_url: String,

    /// Name of the branch, if not the default branch
    pub branch: Option<String>,

    /// Subpath within the repository
    pub subpath: Option<String>,
}

impl FromStr for ParsedVcs {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        let mut subpath: Option<String> = None;
        let branch: Option<String>;
        let repo_url: String;
        let re = Regex::new(r" \[([^] ]+)\]").unwrap();

        let remaining = if let Some(m) = re.find(s) {
            let substr = &m.as_str()[2..m.as_str().len() - 1];
            subpath = Some(substr.to_string());
            format!("{}{}", &s[..m.start()], &s[m.end()..])
        } else {
            s.to_string()
        };

        if let Some(index) = remaining.find(" -b ") {
            let (url, branch_str) = remaining.split_at(index);
            branch = Some(branch_str[4..].to_string());
            repo_url = url.to_string();
        } else {
            branch = None;
            repo_url = remaining;
        }

        Ok(ParsedVcs {
            repo_url,
            branch,
            subpath,
        })
    }
}

impl std::fmt::Display for ParsedVcs {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(&self.repo_url)?;

        if let Some(branch) = &self.branch {
            write!(f, " -b {}", branch)?;
        }

        if let Some(subpath) = &self.subpath {
            write!(f, " [{}]", subpath)?;
        }

        Ok(())
    }
}

/// Version Control System information
#[derive(Debug, Clone)]
pub enum Vcs {
    /// Git repository
    Git {
        /// URL of the repository
        repo_url: String,

        /// Name of the branch, if not the default branch
        branch: Option<String>,

        /// Subpath within the repository
        subpath: Option<String>,
    },
    /// Bazaar branch
    Bzr {
        /// URL of the repository
        repo_url: String,

        /// Subpath within the repository
        subpath: Option<String>,
    },

    /// Mercurial repository
    Hg {
        /// URL of the repository
        repo_url: String,
    },
    /// Subversion repository
    Svn {
        /// URL of the repository, including branch path and subpath
        url: String,
    },
    /// CVS repository
    Cvs {
        /// Root of the CVS repository
        root: String,

        /// Module within the CVS repository
        module: Option<String>,
    },
}

impl Vcs {
    /// Parse a VCS field
    ///
    /// # Arguments
    /// * `name` - Name of the VCS
    /// * `value` - Value of the VCS field
    pub fn from_field(name: &str, value: &str) -> Result<Vcs, String> {
        match name {
            "Git" => {
                let parsed_vcs: ParsedVcs =
                    value.parse::<ParsedVcs>().map_err(|e| e.to_string())?;
                Ok(Vcs::Git {
                    repo_url: parsed_vcs.repo_url,
                    branch: parsed_vcs.branch,
                    subpath: parsed_vcs.subpath,
                })
            }
            "Bzr" => {
                let parsed_vcs: ParsedVcs =
                    value.parse::<ParsedVcs>().map_err(|e| e.to_string())?;
                if parsed_vcs.branch.is_some() {
                    return Err("Invalid branch value for Vcs-Bzr".to_string());
                }
                Ok(Vcs::Bzr {
                    repo_url: parsed_vcs.repo_url,
                    subpath: parsed_vcs.subpath,
                })
            }
            "Hg" => Ok(Vcs::Hg {
                repo_url: value.to_string(),
            }),
            "Svn" => Ok(Vcs::Svn {
                url: value.to_string(),
            }),
            "Cvs" => {
                if let Some((root, module)) = value.split_once(' ') {
                    Ok(Vcs::Cvs {
                        root: root.to_string(),
                        module: Some(module.to_string()),
                    })
                } else {
                    Ok(Vcs::Cvs {
                        root: value.to_string(),
                        module: None,
                    })
                }
            }
            n => Err(format!("Unknown VCS: {}", n)),
        }
    }

    /// Convert the VCS information to a field
    ///
    /// Returns a tuple with the name of the VCS and the value of the field
    pub fn to_field(&self) -> (&str, String) {
        match self {
            Vcs::Git {
                repo_url,
                branch,
                subpath,
            } => (
                "Git",
                ParsedVcs {
                    repo_url: repo_url.clone(),
                    branch: branch.clone(),
                    subpath: subpath.clone(),
                }
                .to_string(),
            ),
            Vcs::Bzr { repo_url, subpath } => (
                "Bzr",
                match subpath {
                    Some(subpath) => format!("{} [{}]", repo_url, subpath),
                    None => repo_url.clone(),
                },
            ),
            Vcs::Hg { repo_url } => ("Hg", repo_url.clone()),
            Vcs::Svn { url } => ("Svn", url.clone()),
            Vcs::Cvs { root, module } => (
                "Cvs",
                match module {
                    Some(module) => format!("{} {}", root, module),
                    None => root.clone(),
                },
            ),
        }
    }

    /// Extract the subpath from the VCS information
    pub fn subpath(&self) -> Option<String> {
        match self {
            Vcs::Git { subpath, .. } => subpath.clone(),
            Vcs::Bzr { subpath, .. } => subpath.clone(),
            _ => None,
        }
    }

    /// Convert the VCS information to a URL that is usable by Breezy
    pub fn to_branch_url(&self) -> Option<String> {
        match self {
            Vcs::Git {
                repo_url,
                branch,
                subpath: _,
                // TODO: Proper URL encoding
            } => Some(format!("{},branch={}", repo_url, branch.as_ref().unwrap())),
            Vcs::Bzr {
                repo_url,
                subpath: _,
            } => Some(repo_url.clone()),
            Vcs::Hg { repo_url } => Some(repo_url.clone()),
            Vcs::Svn { url } => Some(url.clone()),
            _ => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_vcs_info() {
        let vcs_info = ParsedVcs::from_str("https://github.com/jelmer/example").unwrap();
        assert_eq!(vcs_info.repo_url, "https://github.com/jelmer/example");
        assert_eq!(vcs_info.branch, None);
        assert_eq!(vcs_info.subpath, None);
    }

    #[test]
    fn test_vcs_info_with_branch() {
        let vcs_info = ParsedVcs::from_str("https://github.com/jelmer/example -b branch").unwrap();
        assert_eq!(vcs_info.repo_url, "https://github.com/jelmer/example");
        assert_eq!(vcs_info.branch, Some("branch".to_string()));
        assert_eq!(vcs_info.subpath, None);
    }

    #[test]
    fn test_vcs_info_with_subpath() {
        let vcs_info = ParsedVcs::from_str("https://github.com/jelmer/example [subpath]").unwrap();
        assert_eq!(vcs_info.repo_url, "https://github.com/jelmer/example");
        assert_eq!(vcs_info.branch, None);
        assert_eq!(vcs_info.subpath, Some("subpath".to_string()));
    }

    #[test]
    fn test_vcs_info_with_branch_and_subpath() {
        let vcs_info =
            ParsedVcs::from_str("https://github.com/jelmer/example -b branch [subpath]").unwrap();
        assert_eq!(vcs_info.repo_url, "https://github.com/jelmer/example");
        assert_eq!(vcs_info.branch, Some("branch".to_string()));
        assert_eq!(vcs_info.subpath, Some("subpath".to_string()));
    }

    #[test]
    fn test_eq() {
        let vcs_info1 =
            ParsedVcs::from_str("https://github.com/jelmer/example -b branch [subpath]").unwrap();
        let vcs_info2 =
            ParsedVcs::from_str("https://github.com/jelmer/example -b branch [subpath]").unwrap();
        let vcs_info3 =
            ParsedVcs::from_str("https://example.com/jelmer/example -b branch [subpath]").unwrap();

        assert_eq!(vcs_info1, vcs_info2);
        assert_ne!(vcs_info1, vcs_info3);
    }
}
