Editable parser for deb822 style files
======================================

# Example

```rust
use deb822_edit::Deb822;
use std::str::FromStr;

let input = r#"Package: deb822-edit
Maintainer: Jelmer Vernooĳ <jelmer@debian.org>
Section: rust

Package: deb822-edit
Architecture: any
Description: Editable parser for deb822 style files.
  This parser can be used to parse files in the deb822 format, while preserving
  all whitespace and comments. It is based on the [rowan] library, which is a
  lossless parser library for Rust.
"#;

let deb822 = Deb822::from_str(input).unwrap();
assert_eq!(deb822.paragraphs().count(), 2);
```

A derive-macro is also provided for easily defining more Deb822-derived types:

```rust
#[cfg(feature = "derive")]
{
use deb822_edit::{FromDeb822, ToDeb822};

#[derive(FromDeb822, ToDeb822, Debug, PartialEq)]
struct Test {
    #[deb822(field = "Package")]
    package: String,
    architecture: Option<String>,

    #[deb822(field = "Description")]
    description: String,
}

let input = r#"Package: deb822-edit
Architecture: any
Description: Editable parser for deb822 style files.
  This parser can be used to parse files in the deb822 format, while preserving
  all whitespace and comments. It is based on the [rowan] library, which is a
  lossless parser library for Rust.
"#;

use deb822_edit::Paragraph;
use deb822_edit::convert::FromDeb822Paragraph;
let parser: Paragraph = input.parse().unwrap();
let test: Test = FromDeb822Paragraph::from_paragraph(&parser).unwrap();

assert_eq!(test.package, "deb822-edit");
}
```
