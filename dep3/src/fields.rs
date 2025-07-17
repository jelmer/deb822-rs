/// Whether the patch has been forwarded to the upstream project.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Forwarded {
    /// The patch has not been forwarded to the upstream project.
    No,

    /// The patch does not need to be forwarded to the upstream project.
    NotNeeded,

    /// The patch has been forwarded to the upstream project, and the value
    /// provides some reference to the forwarded patch.
    Yes(String),
}

impl std::fmt::Display for Forwarded {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Forwarded::No => f.write_str("no"),
            Forwarded::NotNeeded => f.write_str("not-needed"),
            Forwarded::Yes(s) => f.write_str(s),
        }
    }
}

impl std::str::FromStr for Forwarded {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "no" => Ok(Forwarded::No),
            "not-needed" => Ok(Forwarded::NotNeeded),
            s => Ok(Forwarded::Yes(s.to_string())),
        }
    }
}

/// The category of the origin
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum OriginCategory {
    /// an upstream patch that had to be modified to apply on the current version
    Backport,
    /// a patch created by Debian or another distribution vendor
    Vendor,
    /// a patch cherry-picked from the upstream VCS
    Upstream,

    /// a patch that does not fit in any of the above categories
    Other,
}

impl std::fmt::Display for OriginCategory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OriginCategory::Backport => f.write_str("backport"),
            OriginCategory::Vendor => f.write_str("vendor"),
            OriginCategory::Upstream => f.write_str("upstream"),
            OriginCategory::Other => f.write_str("other"),
        }
    }
}

impl std::str::FromStr for OriginCategory {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "backport" => Ok(OriginCategory::Backport),
            "vendor" => Ok(OriginCategory::Vendor),
            "upstream" => Ok(OriginCategory::Upstream),
            "other" => Ok(OriginCategory::Other),
            _ => Err("invalid origin category"),
        }
    }
}

/// The origin of the patch
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Origin {
    /// The patch was cherry-picked from the upstream VCS
    Commit(String),

    /// Some other origin
    Other(String),
}

impl std::fmt::Display for Origin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Origin::Commit(s) => write!(f, "commit:{}", s),
            Origin::Other(s) => f.write_str(&s.to_string()),
        }
    }
}

impl std::str::FromStr for Origin {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(rest) = s.strip_prefix("commit:") {
            Ok(Origin::Commit(rest.to_string()))
        } else {
            Ok(Origin::Other(s.to_string()))
        }
    }
}

/// Whether the patch has been applied in the upstream project.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum AppliedUpstream {
    /// The patch has not been applied in the upstream project, in the specified commit.
    Commit(String),

    /// The patch has been applied in the upstream project, in the specified reference.
    Other(String),
}

impl std::fmt::Display for AppliedUpstream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AppliedUpstream::Commit(s) => write!(f, "commit:{}", s),
            AppliedUpstream::Other(s) => f.write_str(&s.to_string()),
        }
    }
}

impl std::str::FromStr for AppliedUpstream {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(rest) = s.strip_prefix("commit:") {
            Ok(AppliedUpstream::Commit(rest.to_string()))
        } else {
            Ok(AppliedUpstream::Other(s.to_string()))
        }
    }
}

pub(crate) fn parse_origin(s: &str) -> (Option<OriginCategory>, Origin) {
    // if origin starts with "<category>, " then it is a category

    let mut parts = s.splitn(2, ", ");
    let (category, s) = match parts.next() {
        Some("backport") => (Some(OriginCategory::Backport), parts.next().unwrap_or("")),
        Some("vendor") => (Some(OriginCategory::Vendor), parts.next().unwrap_or("")),
        Some("upstream") => (Some(OriginCategory::Upstream), parts.next().unwrap_or("")),
        Some("other") => (Some(OriginCategory::Other), parts.next().unwrap_or("")),
        None | Some(_) => (None, s),
    };

    if let Some(rest) = s.strip_prefix("commit:") {
        (category, Origin::Commit(rest.to_string()))
    } else {
        (category, Origin::Other(s.to_string()))
    }
}

pub(crate) fn format_origin(category: &Option<OriginCategory>, origin: &Origin) -> String {
    format!(
        "{}{}",
        category.map(|c| c.to_string() + ", ").unwrap_or_default(),
        origin
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_forwarded_display() {
        assert_eq!(Forwarded::No.to_string(), "no");
        assert_eq!(Forwarded::Yes("url".to_string()).to_string(), "url");
        assert_eq!(Forwarded::NotNeeded.to_string(), "not-needed");
    }

    #[test]
    fn test_applied_upstream_display() {
        let commit = AppliedUpstream::Commit("abc123".to_string());
        assert_eq!(commit.to_string(), "commit:abc123");
        
        let other = AppliedUpstream::Other("merged".to_string());
        assert_eq!(other.to_string(), "merged");
    }
}
