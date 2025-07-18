use std::fs;
use std::process::Command;

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

impl Distribution {
    /// Get the current system's distribution information
    pub fn current() -> Result<Distribution, String> {
        // First try lsb_release for distribution ID
        let lsb_id = Command::new("lsb_release").args(["-i", "-s"]).output();

        if let Ok(output) = lsb_id {
            if output.status.success() {
                let distro_id = String::from_utf8_lossy(&output.stdout)
                    .trim()
                    .to_lowercase();

                return Ok(match distro_id.as_str() {
                    "ubuntu" => Distribution::Ubuntu,
                    "debian" => Distribution::Debian,
                    other => Distribution::Other(other.to_owned()),
                });
            }
        }

        // Fall back to /etc/os-release if lsb_release fails
        if let Ok(content) = fs::read_to_string("/etc/os-release") {
            for line in content.lines() {
                if line.starts_with("ID=") {
                    let id = line
                        .trim_start_matches("ID=")
                        .trim_matches('"')
                        .to_lowercase();

                    return Ok(match id.as_str() {
                        "ubuntu" => Distribution::Ubuntu,
                        "debian" => Distribution::Debian,
                        other => Distribution::Other(other.to_owned()),
                    });
                }
            }
        }

        // If all else fails, assume Debian-based
        Ok(Distribution::Other("unknown".to_owned()))
    }

    /// Get default components for this distribution
    pub fn default_components(&self) -> Vec<&'static str> {
        match self {
            Distribution::Ubuntu => vec!["main", "universe"],
            Distribution::Debian => vec!["main"],
            Distribution::Other(_) => vec!["main"],
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

/// Get system information (codename and architecture)
pub fn get_system_info() -> Result<(String, String), String> {
    // Get distribution codename
    let lsb_release = Command::new("lsb_release")
        .args(["-c", "-s"])
        .output()
        .map_err(|e| format!("Failed to run lsb_release: {}", e))?;

    if !lsb_release.status.success() {
        return Err("Failed to determine distribution codename".to_string());
    }

    let codename = String::from_utf8_lossy(&lsb_release.stdout)
        .trim()
        .to_string();

    // Get architecture
    let dpkg_arch = Command::new("dpkg")
        .arg("--print-architecture")
        .output()
        .map_err(|e| format!("Failed to run dpkg: {}", e))?;

    if !dpkg_arch.status.success() {
        return Err("Failed to determine system architecture".to_string());
    }

    let arch = String::from_utf8_lossy(&dpkg_arch.stdout)
        .trim()
        .to_string();

    Ok((codename, arch))
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
            "http://us.archive.ubuntu.com/ubuntu",  // contains ubuntu.com
            "http://mirrors.canonical.com/ubuntu",   // contains canonical.com
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
            "http://ftp.debian.org/debian",  // contains debian.org
            "http://mirrors.debian.org/debian",  // contains debian.org
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
        assert_eq!(Distribution::Ubuntu.default_components(), vec!["main", "universe"]);
        assert_eq!(Distribution::Debian.default_components(), vec!["main"]);
        assert_eq!(Distribution::Other("mint".to_string()).default_components(), vec!["main"]);
    }
}
