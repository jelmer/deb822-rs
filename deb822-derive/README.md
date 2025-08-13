This crate provides a basic proc-macro for converting a Deb822Paragraph
into a Rust struct and vice versa.

You probably want to use the ``deb822_edit`` crate instead,
with the ``derive`` feature enabled.

# Example

```rust
use deb822_edit::Deb822;

#[derive(Deb822)]
struct Foo {
    field1: String,
    field2: Option<String>,
}

let paragraph: deb822::Deb822Paragraph = "field1: value1\nfield2: value2".parse().unwrap();
let foo: Foo = paragraph.into();
```
