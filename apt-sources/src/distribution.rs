use std::fs;

/// Represents a Linux distribution
#[derive(Debug, Clone, PartialEq)]
pub enum Distribution {
    /// Ubuntu Linux
    Ubuntu,
    /// Debian Linux
    Debian,
    /// Other distribution
    Other(String),
}

impl std::fmt::Display for Distribution {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Distribution::Ubuntu => write!(f, "Ubuntu"),
            Distribution::Debian => write!(f, "Debian"),
            Distribution::Other(name) => write!(f, "{}", name),
        }
    }
}

impl Distribution {
    /// Get the current system's distribution information
    ///
    /// Returns None if /etc/os-release is not present or cannot be parsed.
    pub fn current() -> Option<Distribution> {
        let content = fs::read_to_string("/etc/os-release").ok()?;

        for line in content.lines() {
            if line.starts_with("ID=") {
                let id = line
                    .trim_start_matches("ID=")
                    .trim_matches('"')
                    .to_lowercase();

                return Some(match id.as_str() {
                    "ubuntu" => Distribution::Ubuntu,
                    "debian" => Distribution::Debian,
                    other => Distribution::Other(other.to_owned()),
                });
            }
        }

        None
    }

    /// Get default components for this distribution
    pub fn default_components(&self) -> Vec<&'static str> {
        match self {
            Distribution::Ubuntu => vec!["main", "universe"],
            Distribution::Debian => vec!["main"],
            Distribution::Other(_) => vec!["main"],
        }
    }

    /// Get the base name for the main sources file for this distribution
    ///
    /// For example, returns "ubuntu" for Ubuntu, "debian" for Debian.
    /// This can be used to construct filenames like "ubuntu.sources".
    pub fn sources_basename(&self) -> Option<&'static str> {
        match self {
            Distribution::Ubuntu => Some("ubuntu"),
            Distribution::Debian => Some("debian"),
            Distribution::Other(_) => None,
        }
    }

    /// Check if a repository is a main distribution repository
    pub fn is_main_repository(&self, repo: &crate::Repository) -> bool {
        for uri in &repo.uris {
            if let Some(host) = uri.host_str() {
                match self {
                    Distribution::Ubuntu => {
                        if host.contains("ubuntu.com")
                            || host.contains("canonical.com")
                            || host == "archive.ubuntu.com"
                            || host == "security.ubuntu.com"
                            || host == "ports.ubuntu.com"
                        {
                            return true;
                        }
                    }
                    Distribution::Debian => {
                        if host.contains("debian.org")
                            || host == "deb.debian.org"
                            || host == "security.debian.org"
                        {
                            return true;
                        }
                    }
                    _ => {}
                }
            }
        }
        false
    }
}

/// Get system codename from /etc/os-release
///
/// Returns None if /etc/os-release is not present or doesn't contain VERSION_CODENAME.
/// The architecture is always an empty string (APT will use the system's native architecture).
pub fn get_system_info() -> Option<(String, String)> {
    let content = fs::read_to_string("/etc/os-release").ok()?;

    let codename = content
        .lines()
        .find(|line| line.starts_with("VERSION_CODENAME="))
        .map(|line| {
            line.trim_start_matches("VERSION_CODENAME=")
                .trim_matches('"')
                .to_string()
        })?;

    // Return empty string for architecture - APT will use the system's native architecture
    // when the Architectures field is omitted from the sources file
    Some((codename, String::new()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_main_repository() {
        let dist = Distribution::Ubuntu;
        let repo = crate::Repository {
            uris: vec![url::Url::parse("http://archive.ubuntu.com/ubuntu").unwrap()],
            suites: vec!["jammy".to_string()],
            components: Some(vec!["main".to_string()]),
            ..Default::default()
        };
        assert!(dist.is_main_repository(&repo));

        let non_main_repo = crate::Repository {
            uris: vec![url::Url::parse("http://example.com/ubuntu").unwrap()],
            suites: vec!["jammy".to_string()],
            components: Some(vec!["main".to_string()]),
            ..Default::default()
        };
        assert!(!dist.is_main_repository(&non_main_repo));
    }

    #[test]
    fn test_is_main_repository_all_ubuntu_hosts() {
        let dist = Distribution::Ubuntu;

        // Test each Ubuntu host individually
        let ubuntu_hosts = [
            "http://archive.ubuntu.com/ubuntu",
            "http://security.ubuntu.com/ubuntu",
            "http://ports.ubuntu.com/ubuntu-ports",
            "http://us.archive.ubuntu.com/ubuntu", // contains ubuntu.com
            "http://mirrors.canonical.com/ubuntu", // contains canonical.com
        ];

        for host in &ubuntu_hosts {
            let repo = crate::Repository {
                uris: vec![url::Url::parse(host).unwrap()],
                ..Default::default()
            };
            assert!(dist.is_main_repository(&repo), "Failed for host: {}", host);
        }
    }

    #[test]
    fn test_is_main_repository_all_debian_hosts() {
        let dist = Distribution::Debian;

        // Test each Debian host individually
        let debian_hosts = [
            "http://deb.debian.org/debian",
            "http://security.debian.org/debian-security",
            "http://ftp.debian.org/debian",     // contains debian.org
            "http://mirrors.debian.org/debian", // contains debian.org
        ];

        for host in &debian_hosts {
            let repo = crate::Repository {
                uris: vec![url::Url::parse(host).unwrap()],
                ..Default::default()
            };
            assert!(dist.is_main_repository(&repo), "Failed for host: {}", host);
        }
    }

    #[test]
    fn test_is_main_repository_other_distribution() {
        let dist = Distribution::Other("mint".to_string());

        // Other distributions should not match any repository
        let repo = crate::Repository {
            uris: vec![url::Url::parse("http://archive.ubuntu.com/ubuntu").unwrap()],
            ..Default::default()
        };
        assert!(!dist.is_main_repository(&repo));

        let repo2 = crate::Repository {
            uris: vec![url::Url::parse("http://deb.debian.org/debian").unwrap()],
            ..Default::default()
        };
        assert!(!dist.is_main_repository(&repo2));
    }

    #[test]
    fn test_is_main_repository_empty_uris() {
        let dist = Distribution::Ubuntu;
        let repo = crate::Repository {
            uris: vec![],
            ..Default::default()
        };
        assert!(!dist.is_main_repository(&repo));
    }

    #[test]
    fn test_is_main_repository_multiple_uris() {
        let dist = Distribution::Ubuntu;
        let repo = crate::Repository {
            uris: vec![
                url::Url::parse("http://example.com/ubuntu").unwrap(),
                url::Url::parse("http://archive.ubuntu.com/ubuntu").unwrap(),
            ],
            ..Default::default()
        };
        // Should return true if ANY URI matches
        assert!(dist.is_main_repository(&repo));
    }

    #[test]
    fn test_default_components() {
        assert_eq!(
            Distribution::Ubuntu.default_components(),
            vec!["main", "universe"]
        );
        assert_eq!(Distribution::Debian.default_components(), vec!["main"]);
        assert_eq!(
            Distribution::Other("mint".to_string()).default_components(),
            vec!["main"]
        );
    }
}
