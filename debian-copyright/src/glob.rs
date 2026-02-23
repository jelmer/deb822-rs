/// Convert a glob pattern to a regular expression.
pub fn glob_to_regex(glob: &str) -> regex::Regex {
    let mut it = glob.chars();
    let mut r = "^".to_string();

    while let Some(c) = it.next() {
        match c {
            '*' => r.push_str(".*"),
            '?' => r.push('.'),
            '\\' => {
                let c = it.next();
                match c {
                    Some('?') | Some('*') | Some('\\') => {
                        let escaped = regex::escape(&c.unwrap().to_string());
                        r.push_str(&escaped);
                    }
                    Some(x) => {
                        panic!("invalid escape sequence: \\{}", x);
                    }
                    None => {
                        panic!("invalid escape sequence: \\");
                    }
                }
            }
            c => {
                let escaped = regex::escape(&c.to_string());
                r.push_str(&escaped);
            }
        }
    }

    r.push('$');

    regex::Regex::new(r.as_str()).unwrap()
}

/// Calculate the depth of a Files pattern by counting '/' characters.
pub fn pattern_depth(pattern: &str) -> usize {
    pattern.matches('/').count()
}

/// Check if a pattern is a debian/* pattern (should be sorted last by convention).
pub fn is_debian_pattern(pattern: &str) -> bool {
    let trimmed = pattern.trim();
    trimmed.starts_with("debian/") || trimmed == "debian/*"
}

/// Calculate a sort key for a Files pattern.
///
/// Returns `(priority, depth)` where:
/// - priority 0: `*` (always first)
/// - priority 1: normal patterns (sorted by depth)
/// - priority 2: debian/* patterns (always last, then by depth)
///
/// This follows the Debian convention that the `*` wildcard should be first,
/// and `debian/*` patterns should be last in debian/copyright Files paragraphs.
pub fn pattern_sort_key(pattern: &str, depth: usize) -> (u8, usize) {
    let trimmed = pattern.trim();

    if trimmed == "*" {
        (0, 0)
    } else if is_debian_pattern(pattern) {
        (2, depth)
    } else {
        (1, depth)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pattern_depth() {
        assert_eq!(pattern_depth("*"), 0);
        assert_eq!(pattern_depth("src/*"), 1);
        assert_eq!(pattern_depth("src/foo/*"), 2);
        assert_eq!(pattern_depth("a/b/c/d/*"), 4);
        assert_eq!(pattern_depth("debian/*"), 1);
    }

    #[test]
    fn test_is_debian_pattern() {
        assert!(is_debian_pattern("debian/*"));
        assert!(is_debian_pattern("debian/patches/*"));
        assert!(is_debian_pattern(" debian/* "));
        assert!(!is_debian_pattern("*"));
        assert!(!is_debian_pattern("src/*"));
        assert!(!is_debian_pattern("src/debian/*"));
    }

    #[test]
    fn test_pattern_sort_key() {
        // Test wildcard pattern (priority 0)
        assert_eq!(pattern_sort_key("*", 0), (0, 0));
        assert_eq!(pattern_sort_key(" * ", 0), (0, 0));

        // Test normal patterns (priority 1)
        assert_eq!(pattern_sort_key("src/*", 1), (1, 1));
        assert_eq!(pattern_sort_key("src/foo/*", 2), (1, 2));
        assert_eq!(pattern_sort_key("tests/*", 1), (1, 1));

        // Test debian patterns (priority 2)
        assert_eq!(pattern_sort_key("debian/*", 1), (2, 1));
        assert_eq!(pattern_sort_key("debian/patches/*", 2), (2, 2));
    }

    #[test]
    fn test_pattern_sort_key_ordering() {
        // Wildcard comes first
        assert!(pattern_sort_key("*", 0) < pattern_sort_key("src/*", 1));
        assert!(pattern_sort_key("*", 0) < pattern_sort_key("debian/*", 1));

        // Normal patterns come before debian patterns
        assert!(pattern_sort_key("src/*", 1) < pattern_sort_key("debian/*", 1));
        assert!(pattern_sort_key("tests/*", 1) < pattern_sort_key("debian/*", 1));

        // Within same priority, shallower comes before deeper
        assert!(pattern_sort_key("src/*", 1) < pattern_sort_key("src/foo/*", 2));
        assert!(pattern_sort_key("debian/*", 1) < pattern_sort_key("debian/patches/*", 2));

        // Debian patterns come last even with same depth as normal patterns
        assert!(pattern_sort_key("src/*", 1) < pattern_sort_key("debian/*", 1));
    }

    #[test]
    fn test_simple() {
        let r = super::glob_to_regex("*.rs");
        assert!(r.is_match("foo.rs"));
        assert!(r.is_match("bar.rs"));
        assert!(!r.is_match("foo.rs.bak"));
        assert!(!r.is_match("foo"));
    }

    #[test]
    fn test_single_char() {
        let r = super::glob_to_regex("?.rs");
        assert!(r.is_match("a.rs"));
        assert!(r.is_match("b.rs"));
        assert!(!r.is_match("foo.rs"));
        assert!(!r.is_match("foo"));
    }

    #[test]
    fn test_escape() {
        let r = super::glob_to_regex(r"\?.rs");
        assert!(r.is_match("?.rs"));
        assert!(!r.is_match("a.rs"));
        assert!(!r.is_match("b.rs"));

        let r = super::glob_to_regex(r"\*.rs");
        assert!(r.is_match("*.rs"));
        assert!(!r.is_match("a.rs"));
        assert!(!r.is_match("b.rs"));

        let r = super::glob_to_regex(r"\\?.rs");
        assert!(r.is_match("\\a.rs"));
        assert!(r.is_match("\\b.rs"));
        assert!(!r.is_match("a.rs"));
    }

    #[should_panic]
    #[test]
    fn test_invalid_escape() {
        super::glob_to_regex(r"\x.rs");
    }

    #[should_panic]
    #[test]
    fn test_invalid_escape2() {
        super::glob_to_regex(r"\");
    }
}
