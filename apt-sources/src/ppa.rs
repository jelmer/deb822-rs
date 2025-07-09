//!  This module provides functionality for handling Personal Package Archives (PPAs) in a
//!  Debian/Ubuntu context.

use url::Url;

/// Default URL for Launchpad PPAs
pub const LAUNCHPAD_PPA_URL: &str = "https://ppa.launchpadcontent.net";

/// Valid components for PPAs
pub const VALID_PPA_COMPONENTS: &[&str] = &["main", "main/debug"];

/// Information about a PPA (Personal Package Archive)
#[derive(Debug, Clone)]
pub struct PpaInfo {
    /// The PPA owner's username
    pub user: String,
    /// The PPA name
    pub name: String,
}

impl PpaInfo {
    /// Parse a PPA specification string (e.g., "ppa:user/ppa-name")
    pub fn parse(ppa_spec: &str) -> Result<PpaInfo, String> {
        if !ppa_spec.starts_with("ppa:") {
            return Err("Not a PPA format".to_string());
        }

        let ppa_part = &ppa_spec[4..];
        let parts: Vec<&str> = ppa_part.split('/').collect();

        if parts.len() != 2 {
            return Err("Invalid PPA format. Expected ppa:user/ppa-name".to_string());
        }

        Ok(PpaInfo {
            user: parts[0].to_string(),
            name: parts[1].to_string(),
        })
    }

    /// Generate the repository URL for this PPA
    pub fn repository_url(&self, _codename: &str) -> Result<Url, String> {
        Url::parse(&format!(
            "{}/{}/{}/ubuntu",
            LAUNCHPAD_PPA_URL, self.user, self.name
        ))
        .map_err(|e| format!("Failed to construct PPA URL: {}", e))
    }

    /// Generate a filename for this PPA
    pub fn filename(&self, extension: &str) -> String {
        format!("{}-ubuntu-{}.{}", self.user, self.name, extension)
    }
}

/// Validate PPA components
pub fn validate_ppa_components(components: &[String]) -> Result<(), String> {
    for component in components {
        if !VALID_PPA_COMPONENTS.contains(&component.as_str()) {
            return Err(format!(
                "Invalid component '{}' for PPA.\n\
                 Valid components are: {}\n\
                 Suggestion: Use 'main' for regular packages or 'main/debug' for debug symbols.",
                component,
                VALID_PPA_COMPONENTS.join(", ")
            ));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_ppa_format() {
        // Valid PPA
        let ppa = PpaInfo::parse("ppa:user/repo").unwrap();
        assert_eq!(ppa.user, "user");
        assert_eq!(ppa.name, "repo");

        // Invalid formats
        assert!(PpaInfo::parse("not-a-ppa").is_err());
        assert!(PpaInfo::parse("ppa:invalid").is_err());
        assert!(PpaInfo::parse("ppa:too/many/parts").is_err());
    }

    #[test]
    fn test_validate_ppa_components() {
        assert!(validate_ppa_components(&["main".to_string()]).is_ok());
        assert!(validate_ppa_components(&["main/debug".to_string()]).is_ok());
        assert!(validate_ppa_components(&["invalid".to_string()]).is_err());
    }
}
