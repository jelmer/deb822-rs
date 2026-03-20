/// A compiled DEP-5 glob pattern that can efficiently match many paths.
///
/// The pattern uses the glob syntax defined by the DEP-5 specification:
/// `*` matches any sequence of characters, `?` matches a single character,
/// and backslash escapes `*`, `?` and `\`.
///
/// # Examples
///
/// ```
/// let pat = debian_copyright::GlobPattern::new("src/*.rs");
/// assert!(pat.is_match("src/main.rs"));
/// assert!(!pat.is_match("lib/main.rs"));
/// ```
pub struct GlobPattern(regex::Regex);

impl GlobPattern {
    /// Compile a DEP-5 glob pattern.
    pub fn new(pattern: &str) -> Self {
        Self(glob_to_regex_inner(pattern))
    }

    /// Check whether a path matches this pattern.
    pub fn is_match(&self, path: &str) -> bool {
        self.0.is_match(path)
    }
}

/// Convert a glob pattern to a regular expression.
#[deprecated(since = "0.1.46", note = "use GlobPattern instead")]
pub fn glob_to_regex(glob: &str) -> regex::Regex {
    glob_to_regex_inner(glob)
}

fn glob_to_regex_inner(glob: &str) -> regex::Regex {
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

#[cfg(test)]
mod tests {
    #[allow(deprecated)]
    #[test]
    fn test_simple() {
        let r = super::glob_to_regex("*.rs");
        assert!(r.is_match("foo.rs"));
        assert!(r.is_match("bar.rs"));
        assert!(!r.is_match("foo.rs.bak"));
        assert!(!r.is_match("foo"));
    }

    #[allow(deprecated)]
    #[test]
    fn test_single_char() {
        let r = super::glob_to_regex("?.rs");
        assert!(r.is_match("a.rs"));
        assert!(r.is_match("b.rs"));
        assert!(!r.is_match("foo.rs"));
        assert!(!r.is_match("foo"));
    }

    #[allow(deprecated)]
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

    #[allow(deprecated)]
    #[should_panic]
    #[test]
    fn test_invalid_escape() {
        super::glob_to_regex(r"\x.rs");
    }

    #[allow(deprecated)]
    #[should_panic]
    #[test]
    fn test_invalid_escape2() {
        super::glob_to_regex(r"\");
    }

    #[test]
    fn test_glob_pattern_wildcard() {
        let pat = super::GlobPattern::new("src/*.rs");
        assert!(pat.is_match("src/main.rs"));
        assert!(pat.is_match("src/lib.rs"));
        assert!(!pat.is_match("lib/main.rs"));
        assert!(!pat.is_match("src/main.rs.bak"));
    }

    #[test]
    fn test_glob_pattern_deep_wildcard() {
        let pat = super::GlobPattern::new("src/*");
        assert!(pat.is_match("src/foo"));
        assert!(pat.is_match("src/foo/bar.rs"));
        assert!(!pat.is_match("lib/foo"));
    }

    #[test]
    fn test_glob_pattern_question_mark() {
        let pat = super::GlobPattern::new("file?.txt");
        assert!(pat.is_match("file1.txt"));
        assert!(pat.is_match("fileA.txt"));
        assert!(!pat.is_match("file10.txt"));
        assert!(!pat.is_match("file.txt"));
    }

    #[test]
    fn test_glob_pattern_literal() {
        let pat = super::GlobPattern::new("LICENSE");
        assert!(pat.is_match("LICENSE"));
        assert!(!pat.is_match("LICENSE.md"));
        assert!(!pat.is_match("NOLICENSE"));
    }

    #[test]
    fn test_glob_pattern_escaped_star() {
        let pat = super::GlobPattern::new(r"\*.txt");
        assert!(pat.is_match("*.txt"));
        assert!(!pat.is_match("foo.txt"));
    }

    #[test]
    fn test_glob_pattern_escaped_question() {
        let pat = super::GlobPattern::new(r"\?.txt");
        assert!(pat.is_match("?.txt"));
        assert!(!pat.is_match("a.txt"));
    }

    #[test]
    fn test_glob_pattern_escaped_backslash() {
        let pat = super::GlobPattern::new(r"\\foo");
        assert!(pat.is_match(r"\foo"));
        assert!(!pat.is_match("foo"));
    }

    #[should_panic]
    #[test]
    fn test_glob_pattern_invalid_escape() {
        super::GlobPattern::new(r"\x");
    }

    #[should_panic]
    #[test]
    fn test_glob_pattern_trailing_backslash() {
        super::GlobPattern::new(r"\");
    }

    #[test]
    fn test_glob_pattern_regex_special_chars() {
        let pat = super::GlobPattern::new("file(1).txt");
        assert!(pat.is_match("file(1).txt"));
        assert!(!pat.is_match("file1.txt"));
    }
}
