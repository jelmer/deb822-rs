use url::Url;

/// Strip authentication information from a URL and return it separately
pub fn strip_auth_from_url(url: &Url) -> (Url, Option<(String, String)>) {
    let auth = if !url.username().is_empty() {
        Some((
            url.username().to_string(),
            url.password().unwrap_or("").to_string(),
        ))
    } else {
        None
    };

    let mut clean_url = url.clone();
    if auth.is_some() {
        clean_url.set_username("").ok();
        clean_url.set_password(None).ok();
    }

    (clean_url, auth)
}

/// Generate a sanitized filename from a URL
pub fn generate_filename_from_url(url: &Url, extension: &str) -> String {
    let host = url.host_str().unwrap_or("unknown");
    let path = url.path();

    // Remove leading/trailing slashes and replace internal slashes
    let path_part = path
        .trim_start_matches('/')
        .trim_end_matches('/')
        .replace('/', "-");

    let base = if path_part.is_empty() {
        host.to_string()
    } else {
        format!("{}-{}", host, path_part)
    };

    // Sanitize the filename
    let sanitized = base.replace(':', "-").replace(' ', "-").to_lowercase();

    format!("{}.{}", sanitized, extension)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strip_auth_from_url() {
        let url = Url::parse("https://user:pass@example.com/path").unwrap();
        let (clean_url, auth) = strip_auth_from_url(&url);

        assert_eq!(clean_url.as_str(), "https://example.com/path");
        assert_eq!(auth, Some(("user".to_string(), "pass".to_string())));

        // URL without auth
        let url = Url::parse("https://example.com/path").unwrap();
        let (clean_url, auth) = strip_auth_from_url(&url);

        assert_eq!(clean_url.as_str(), "https://example.com/path");
        assert_eq!(auth, None);
    }

    #[test]
    fn test_generate_filename_from_url() {
        let url = Url::parse("https://example.com/ubuntu").unwrap();
        assert_eq!(
            generate_filename_from_url(&url, "sources"),
            "example.com-ubuntu.sources"
        );

        let url = Url::parse("https://ppa.launchpad.net/user/repo/ubuntu").unwrap();
        assert_eq!(
            generate_filename_from_url(&url, "list"),
            "ppa.launchpad.net-user-repo-ubuntu.list"
        );
    }
}
