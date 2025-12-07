use deb822_derive::{FromDeb822, ToDeb822};
use deb822_fast::convert::ToDeb822Paragraph;
use deb822_fast::Paragraph;

#[derive(Debug, FromDeb822, ToDeb822)]
struct SingleLineTest {
    #[deb822(field = "Name", single_line)]
    name: String,
}

#[derive(Debug, FromDeb822, ToDeb822)]
struct MultiLineTest {
    #[deb822(field = "Description", multi_line)]
    description: String,
}

#[derive(Debug, FromDeb822, ToDeb822)]
struct FoldedTest {
    #[deb822(field = "Depends", folded)]
    depends: String,
}

#[test]
fn test_single_line_valid() {
    let test = SingleLineTest {
        name: "test-package".to_string(),
    };

    let para: Paragraph = test.to_paragraph();
    assert_eq!(para.get("Name"), Some("test-package"));
}

#[test]
#[should_panic(expected = "Field 'Name' is marked as single_line but contains newlines")]
fn test_single_line_with_newlines_panics() {
    let test = SingleLineTest {
        name: "test\npackage".to_string(),
    };

    // This should panic
    let _para: Paragraph = test.to_paragraph();
}

#[test]
fn test_multi_line_single_line_value() {
    let test = MultiLineTest {
        description: "A simple description".to_string(),
    };

    let para: Paragraph = test.to_paragraph();
    assert_eq!(para.get("Description"), Some("A simple description"));
}

#[test]
fn test_multi_line_adds_spaces_to_continuation_lines() {
    let test = MultiLineTest {
        description: "First line\nSecond line\nThird line".to_string(),
    };

    let para: Paragraph = test.to_paragraph();
    let value = para.get("Description").unwrap();

    // Should add space to continuation lines
    assert_eq!(value, "First line\n Second line\n Third line");
}

#[test]
fn test_multi_line_always_adds_spaces() {
    let test = MultiLineTest {
        description: "First line\nAlready has text\n  Two spaces".to_string(),
    };

    let para: Paragraph = test.to_paragraph();
    let value = para.get("Description").unwrap();

    // Should always add space to continuation lines
    assert_eq!(value, "First line\n Already has text\n   Two spaces");
}

#[test]
fn test_multi_line_empty_lines_become_dot() {
    let test = MultiLineTest {
        description: "First line\n\nThird line".to_string(),
    };

    let para: Paragraph = test.to_paragraph();
    let value = para.get("Description").unwrap();

    // Empty lines should become " ."
    assert_eq!(value, "First line\n .\n Third line");
}

#[test]
fn test_folded_single_line() {
    let test = FoldedTest {
        depends: "libc6".to_string(),
    };

    let para: Paragraph = test.to_paragraph();
    assert_eq!(para.get("Depends"), Some("libc6"));
}

#[test]
fn test_folded_strips_whitespace_and_joins() {
    let test = FoldedTest {
        depends: "  libc6  \n  libssl3  \n  zlib1g  ".to_string(),
    };

    let para: Paragraph = test.to_paragraph();
    let value = para.get("Depends").unwrap();

    // Should strip whitespace from each line and join with spaces
    assert_eq!(value, "libc6 libssl3 zlib1g");
}

#[test]
fn test_folded_filters_empty_lines() {
    let test = FoldedTest {
        depends: "libc6\n\nlibssl3\n  \nzlib1g".to_string(),
    };

    let para: Paragraph = test.to_paragraph();
    let value = para.get("Depends").unwrap();

    // Should filter out empty lines
    assert_eq!(value, "libc6 libssl3 zlib1g");
}

#[test]
fn test_update_paragraph_single_line() {
    let test = SingleLineTest {
        name: "updated-package".to_string(),
    };

    let mut para: Paragraph = vec![].into();
    test.update_paragraph(&mut para);

    assert_eq!(para.get("Name"), Some("updated-package"));
}

#[test]
fn test_update_paragraph_multi_line() {
    let test = MultiLineTest {
        description: "Line one\nLine two".to_string(),
    };

    let mut para: Paragraph = vec![].into();
    test.update_paragraph(&mut para);

    let value = para.get("Description").unwrap();
    assert_eq!(value, "Line one\n Line two");
}

#[test]
fn test_update_paragraph_folded() {
    let test = FoldedTest {
        depends: "  pkg1  \n  pkg2  ".to_string(),
    };

    let mut para: Paragraph = vec![].into();
    test.update_paragraph(&mut para);

    let value = para.get("Depends").unwrap();
    assert_eq!(value, "pkg1 pkg2");
}
