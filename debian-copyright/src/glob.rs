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

#[cfg(test)]
mod tests {
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
