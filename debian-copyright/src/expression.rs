//! License expression parsing for DEP-5 copyright files.
//!
//! License expressions combine license names with `or`, `and`, and `with` operators.
//! `and` binds tighter than `or`. A comma before an operator lowers its precedence,
//! e.g. `A or B, and C` means `(A or B) and C`.
//!
//! The `with` keyword attaches an exception to the preceding license name
//! (e.g. `GPL-2+ with OpenSSL-exception`).

/// A parsed license expression from a DEP-5 copyright file.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum LicenseExpr {
    /// A single license name, e.g. `MIT`.
    Name(String),

    /// A license with an exception, e.g. `GPL-2+ with OpenSSL-exception`.
    WithException(String, String),

    /// All of these licenses apply simultaneously.
    And(Vec<LicenseExpr>),

    /// Any one of these licenses may be chosen.
    Or(Vec<LicenseExpr>),
}

impl LicenseExpr {
    /// Parse a license expression string.
    ///
    /// # Examples
    ///
    /// ```
    /// use debian_copyright::LicenseExpr;
    ///
    /// let expr = LicenseExpr::parse("GPL-2+ or MIT");
    /// assert_eq!(expr, LicenseExpr::Or(vec![
    ///     LicenseExpr::Name("GPL-2+".to_string()),
    ///     LicenseExpr::Name("MIT".to_string()),
    /// ]));
    ///
    /// let expr = LicenseExpr::parse("GPL-2+ with OpenSSL-exception");
    /// assert_eq!(expr, LicenseExpr::WithException(
    ///     "GPL-2+".to_string(),
    ///     "OpenSSL-exception".to_string(),
    /// ));
    /// ```
    pub fn parse(input: &str) -> Self {
        let tokens = tokenize(input);
        if tokens.is_empty() {
            return LicenseExpr::Name(String::new());
        }
        parse_expr(&tokens)
    }

    /// Returns the individual license names contained in this expression.
    ///
    /// For `WithException` variants, only the license name is returned,
    /// not the exception name.
    pub fn license_names(&self) -> Vec<&str> {
        let mut names = Vec::new();
        self.collect_names(&mut names);
        names
    }

    fn collect_names<'a>(&'a self, names: &mut Vec<&'a str>) {
        match self {
            LicenseExpr::Name(n) => names.push(n),
            LicenseExpr::WithException(n, _) => names.push(n),
            LicenseExpr::And(exprs) | LicenseExpr::Or(exprs) => {
                for expr in exprs {
                    expr.collect_names(names);
                }
            }
        }
    }
}

impl std::fmt::Display for LicenseExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LicenseExpr::Name(n) => f.write_str(n),
            LicenseExpr::WithException(n, e) => write!(f, "{} with {}", n, e),
            LicenseExpr::And(exprs) => {
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        f.write_str(" and ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                Ok(())
            }
            LicenseExpr::Or(exprs) => {
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        f.write_str(" or ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Token {
    Word(String),
    Or,
    And,
    With,
    Comma,
}

fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    for word in input.split_whitespace() {
        let (word, has_comma) = if let Some(stripped) = word.strip_suffix(',') {
            (stripped, true)
        } else {
            (word, false)
        };

        if !word.is_empty() {
            if word.eq_ignore_ascii_case("or") {
                tokens.push(Token::Or);
            } else if word.eq_ignore_ascii_case("and") {
                tokens.push(Token::And);
            } else if word.eq_ignore_ascii_case("with") {
                tokens.push(Token::With);
            } else {
                tokens.push(Token::Word(word.to_string()));
            }
        }

        if has_comma {
            tokens.push(Token::Comma);
        }
    }
    tokens
}

/// Parse a single license term: a name optionally followed by `with <exception>`.
/// The exception after `with` consumes all words until the next `or`, `and`, comma, or end.
fn parse_term(tokens: &[Token], pos: &mut usize) -> LicenseExpr {
    let name = match tokens.get(*pos) {
        Some(Token::Word(w)) => {
            *pos += 1;
            w.clone()
        }
        _ => return LicenseExpr::Name(String::new()),
    };

    if matches!(tokens.get(*pos), Some(Token::With)) {
        *pos += 1;
        let mut exception_parts = Vec::new();
        while *pos < tokens.len() {
            match &tokens[*pos] {
                Token::Word(w) => {
                    exception_parts.push(w.clone());
                    *pos += 1;
                }
                _ => break,
            }
        }
        LicenseExpr::WithException(name, exception_parts.join(" "))
    } else {
        LicenseExpr::Name(name)
    }
}

/// Parse a token stream into a `LicenseExpr`.
///
/// Handles comma-lowered precedence by splitting on `, and` / `, or` first,
/// then parsing each segment with normal precedence (`and` > `or`).
fn parse_expr(tokens: &[Token]) -> LicenseExpr {
    // Split into segments at comma boundaries (comma + operator = low precedence).
    let mut segments: Vec<(Vec<Token>, Option<Token>)> = Vec::new();
    let mut current: Vec<Token> = Vec::new();

    let mut i = 0;
    while i < tokens.len() {
        if tokens[i] == Token::Comma {
            if i + 1 < tokens.len() && matches!(tokens[i + 1], Token::Or | Token::And) {
                let op = tokens[i + 1].clone();
                segments.push((std::mem::take(&mut current), Some(op)));
                i += 2;
            } else {
                i += 1;
            }
        } else {
            current.push(tokens[i].clone());
            i += 1;
        }
    }
    if !current.is_empty() {
        segments.push((current, None));
    }

    if segments.len() == 1 {
        return parse_segment(&segments[0].0);
    }

    // Group segments by their joining low-precedence operator.
    // Low-precedence `and` binds tighter than low-precedence `or`.
    // First pass: group consecutive And-joined segments.
    let mut and_groups: Vec<Vec<LicenseExpr>> = vec![vec![parse_segment(&segments[0].0)]];
    let mut joining_ops: Vec<Token> = Vec::new();

    for i in 1..segments.len() {
        let preceding_op = segments[i - 1].1.as_ref().unwrap_or(&Token::Or);
        if matches!(preceding_op, Token::And) {
            and_groups
                .last_mut()
                .unwrap()
                .push(parse_segment(&segments[i].0));
        } else {
            joining_ops.push(Token::Or);
            and_groups.push(vec![parse_segment(&segments[i].0)]);
        }
    }

    let flattened: Vec<LicenseExpr> = and_groups
        .into_iter()
        .map(|group| {
            if group.len() == 1 {
                group.into_iter().next().unwrap()
            } else {
                LicenseExpr::And(group)
            }
        })
        .collect();

    if flattened.len() == 1 {
        flattened.into_iter().next().unwrap()
    } else {
        LicenseExpr::Or(flattened)
    }
}

/// Parse a segment (no comma-lowered operators) with normal precedence: `and` > `or`.
fn parse_segment(tokens: &[Token]) -> LicenseExpr {
    // Split on `or` (lower precedence), then each part on `and`.
    let mut or_groups: Vec<Vec<Token>> = vec![Vec::new()];
    for tok in tokens {
        if *tok == Token::Or {
            or_groups.push(Vec::new());
        } else {
            or_groups.last_mut().unwrap().push(tok.clone());
        }
    }

    let or_exprs: Vec<LicenseExpr> = or_groups
        .into_iter()
        .map(|group| {
            let mut and_groups: Vec<Vec<Token>> = vec![Vec::new()];
            for tok in &group {
                if *tok == Token::And {
                    and_groups.push(Vec::new());
                } else {
                    and_groups.last_mut().unwrap().push(tok.clone());
                }
            }

            let and_exprs: Vec<LicenseExpr> = and_groups
                .into_iter()
                .map(|toks| {
                    let mut pos = 0;
                    parse_term(&toks, &mut pos)
                })
                .collect();

            if and_exprs.len() == 1 {
                and_exprs.into_iter().next().unwrap()
            } else {
                LicenseExpr::And(and_exprs)
            }
        })
        .collect();

    if or_exprs.len() == 1 {
        or_exprs.into_iter().next().unwrap()
    } else {
        LicenseExpr::Or(or_exprs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_name() {
        assert_eq!(LicenseExpr::parse("MIT"), LicenseExpr::Name("MIT".into()));
    }

    #[test]
    fn test_or() {
        assert_eq!(
            LicenseExpr::parse("GPL-2+ or MIT"),
            LicenseExpr::Or(vec![
                LicenseExpr::Name("GPL-2+".into()),
                LicenseExpr::Name("MIT".into()),
            ])
        );
    }

    #[test]
    fn test_and() {
        assert_eq!(
            LicenseExpr::parse("Apache-2.0 and BSD-3-clause"),
            LicenseExpr::And(vec![
                LicenseExpr::Name("Apache-2.0".into()),
                LicenseExpr::Name("BSD-3-clause".into()),
            ])
        );
    }

    #[test]
    fn test_with_exception() {
        assert_eq!(
            LicenseExpr::parse("GPL-2+ with OpenSSL-exception"),
            LicenseExpr::WithException("GPL-2+".into(), "OpenSSL-exception".into())
        );
    }

    #[test]
    fn test_with_multi_word_exception() {
        assert_eq!(
            LicenseExpr::parse("GPL-2+ with Autoconf exception"),
            LicenseExpr::WithException("GPL-2+".into(), "Autoconf exception".into())
        );
    }

    #[test]
    fn test_with_exception_then_or() {
        assert_eq!(
            LicenseExpr::parse("GPL-2+ with OpenSSL-exception or MIT"),
            LicenseExpr::Or(vec![
                LicenseExpr::WithException("GPL-2+".into(), "OpenSSL-exception".into()),
                LicenseExpr::Name("MIT".into()),
            ])
        );
    }

    #[test]
    fn test_and_binds_tighter_than_or() {
        // A or B and C → A or (B and C)
        assert_eq!(
            LicenseExpr::parse("A or B and C"),
            LicenseExpr::Or(vec![
                LicenseExpr::Name("A".into()),
                LicenseExpr::And(vec![
                    LicenseExpr::Name("B".into()),
                    LicenseExpr::Name("C".into()),
                ]),
            ])
        );
    }

    #[test]
    fn test_comma_lowers_precedence() {
        // A or B, and C → (A or B) and C
        assert_eq!(
            LicenseExpr::parse("A or B, and C"),
            LicenseExpr::And(vec![
                LicenseExpr::Or(vec![
                    LicenseExpr::Name("A".into()),
                    LicenseExpr::Name("B".into()),
                ]),
                LicenseExpr::Name("C".into()),
            ])
        );
    }

    #[test]
    fn test_case_insensitive_operators() {
        assert_eq!(
            LicenseExpr::parse("GPL-2+ OR MIT"),
            LicenseExpr::Or(vec![
                LicenseExpr::Name("GPL-2+".into()),
                LicenseExpr::Name("MIT".into()),
            ])
        );
    }

    #[test]
    fn test_license_names() {
        let expr = LicenseExpr::parse("GPL-2+ or MIT and BSD-3-clause");
        assert_eq!(expr.license_names(), vec!["GPL-2+", "MIT", "BSD-3-clause"]);
    }

    #[test]
    fn test_license_names_with_exception() {
        let expr = LicenseExpr::parse("GPL-2+ with OpenSSL-exception or MIT");
        assert_eq!(expr.license_names(), vec!["GPL-2+", "MIT"]);
    }

    #[test]
    fn test_display_round_trip_simple() {
        let input = "GPL-2+ or MIT";
        let expr = LicenseExpr::parse(input);
        assert_eq!(expr.to_string(), input);
    }

    #[test]
    fn test_display_with_exception() {
        let input = "GPL-2+ with OpenSSL-exception";
        let expr = LicenseExpr::parse(input);
        assert_eq!(expr.to_string(), input);
    }

    #[test]
    fn test_three_way_or() {
        assert_eq!(
            LicenseExpr::parse("GPL-1+ or Artistic or Perl"),
            LicenseExpr::Or(vec![
                LicenseExpr::Name("GPL-1+".into()),
                LicenseExpr::Name("Artistic".into()),
                LicenseExpr::Name("Perl".into()),
            ])
        );
    }
}
