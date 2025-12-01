//! Conversion between Deb822-like paragraphs and Rust objects.

/// Abstract trait for accessing and modifying key-value pairs in a paragraph.
pub trait Deb822LikeParagraph: FromIterator<(String, String)> {
    /// Get the value for the given key.
    fn get(&self, key: &str) -> Option<String>;

    /// Insert a key-value pair.
    fn set(&mut self, key: &str, value: &str);

    /// Remove a key-value pair.
    fn remove(&mut self, key: &str);

    /// Iterate over all key-value pairs in the paragraph.
    fn iter(&self) -> impl Iterator<Item = (String, String)>;
}

impl Deb822LikeParagraph for crate::Paragraph {
    fn get(&self, key: &str) -> Option<String> {
        crate::Paragraph::get(self, key).map(|v| v.to_string())
    }

    fn set(&mut self, key: &str, value: &str) {
        crate::Paragraph::set(self, key, value);
    }

    fn remove(&mut self, key: &str) {
        crate::Paragraph::remove(self, key);
    }

    fn iter(&self) -> impl Iterator<Item = (String, String)> {
        crate::Paragraph::iter(self).map(|(k, v)| (k.to_string(), v.to_string()))
    }
}

/// Convert a paragraph to this object.
pub trait FromDeb822Paragraph<P: Deb822LikeParagraph> {
    /// Convert a paragraph to this object.
    fn from_paragraph(paragraph: &P) -> Result<Self, String>
    where
        Self: Sized;
}

/// Convert this object to a paragraph.
pub trait ToDeb822Paragraph<P: Deb822LikeParagraph> {
    /// Convert this object to a paragraph.
    fn to_paragraph(&self) -> P;

    /// Update the given paragraph with the values from this object.
    fn update_paragraph(&self, paragraph: &mut P);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_trait_impl_directly() {
        // Test the trait methods directly to improve coverage
        let mut para = crate::Paragraph {
            fields: vec![crate::Field {
                name: "Test".to_string(),
                value: "Value".to_string(),
            }],
        };

        // Test Deb822LikeParagraph::get
        let result: Option<String> = Deb822LikeParagraph::get(&para, "Test");
        assert_eq!(result, Some("Value".to_string()));

        // Test Deb822LikeParagraph::set
        Deb822LikeParagraph::set(&mut para, "Test", "NewValue");
        assert_eq!(para.get("Test"), Some("NewValue"));

        // Test Deb822LikeParagraph::remove
        Deb822LikeParagraph::remove(&mut para, "Test");
        assert_eq!(para.get("Test"), None);
    }

    #[test]
    fn test_iter() {
        // Test the iter() method
        let para = crate::Paragraph {
            fields: vec![
                crate::Field {
                    name: "First".to_string(),
                    value: "Value1".to_string(),
                },
                crate::Field {
                    name: "Second".to_string(),
                    value: "Value2".to_string(),
                },
                crate::Field {
                    name: "Third".to_string(),
                    value: "Value3".to_string(),
                },
            ],
        };

        let collected: Vec<(String, String)> = Deb822LikeParagraph::iter(&para).collect();
        assert_eq!(collected.len(), 3);
        assert_eq!(collected[0], ("First".to_string(), "Value1".to_string()));
        assert_eq!(collected[1], ("Second".to_string(), "Value2".to_string()));
        assert_eq!(collected[2], ("Third".to_string(), "Value3".to_string()));

        // Test with empty paragraph
        let empty_para = crate::Paragraph { fields: vec![] };
        let empty_collected: Vec<(String, String)> =
            Deb822LikeParagraph::iter(&empty_para).collect();
        assert_eq!(empty_collected.len(), 0);
    }

    #[test]
    fn test_deb822like_paragraph_impl() {
        // Create mock crate::Paragraph for tests
        let mut para = crate::Paragraph {
            fields: vec![crate::Field {
                name: "Name".to_string(),
                value: "Test".to_string(),
            }],
        };

        // Test get() - this calls the implementation on line 16-17
        assert_eq!(para.get("Name"), Some("Test"));
        assert_eq!(para.get("NonExistent"), None);

        // Test set() - this calls the implementation on line 20-21
        para.set("Name", "NewValue");
        assert_eq!(para.get("Name"), Some("NewValue"));

        // Test set() with new key
        para.set("NewKey", "Value");
        assert_eq!(para.get("NewKey"), Some("Value"));

        // Test remove() - this calls the implementation on line 24-25
        para.remove("Name");
        assert_eq!(para.get("Name"), None);
        assert_eq!(para.get("NewKey"), Some("Value"));

        // Create a new paragraph with multiple fields of the same name
        let mut para = crate::Paragraph {
            fields: vec![
                crate::Field {
                    name: "Duplicate".to_string(),
                    value: "Value1".to_string(),
                },
                crate::Field {
                    name: "Duplicate".to_string(),
                    value: "Value2".to_string(),
                },
            ],
        };

        // Test remove() removes all matches
        para.remove("Duplicate");
        assert_eq!(para.get("Duplicate"), None);
        assert_eq!(para.fields.len(), 0);
    }

    #[cfg(feature = "derive")]
    mod derive {
        use super::*;
        use crate as deb822_fast;
        use crate::{FromDeb822, ToDeb822};

        #[test]
        fn test_derive() {
            #[derive(ToDeb822)]
            struct Foo {
                bar: String,
                baz: i32,
                blah: Option<String>,
            }

            let foo = Foo {
                bar: "hello".to_string(),
                baz: 42,
                blah: None,
            };

            let paragraph: crate::Paragraph = foo.to_paragraph();
            assert_eq!(paragraph.get("bar"), Some("hello"));
            assert_eq!(paragraph.get("baz"), Some("42"));
            assert_eq!(paragraph.get("blah"), None);
        }

        #[test]
        fn test_optional_missing() {
            #[derive(ToDeb822)]
            struct Foo {
                bar: String,
                baz: Option<String>,
            }

            let foo = Foo {
                bar: "hello".to_string(),
                baz: None,
            };

            let paragraph: crate::Paragraph = foo.to_paragraph();
            assert_eq!(paragraph.get("bar"), Some("hello"));
            assert_eq!(paragraph.get("baz"), None);

            assert_eq!("bar: hello\n", paragraph.to_string());
        }

        #[test]
        fn test_deserialize_with() {
            let mut para: crate::Paragraph = "bar: bar\n# comment\nbaz: blah\n".parse().unwrap();

            fn to_bool(s: &str) -> Result<bool, String> {
                Ok(s == "ja")
            }

            fn from_bool(s: &bool) -> String {
                if *s {
                    "ja".to_string()
                } else {
                    "nee".to_string()
                }
            }

            #[derive(FromDeb822, ToDeb822)]
            struct Foo {
                bar: String,
                #[deb822(deserialize_with = to_bool, serialize_with = from_bool)]
                baz: bool,
            }

            let mut foo: Foo = Foo::from_paragraph(&para).unwrap();
            assert_eq!(foo.bar, "bar");
            assert!(!foo.baz);

            foo.bar = "new".to_string();

            foo.update_paragraph(&mut para);

            assert_eq!(para.get("bar"), Some("new"));
            assert_eq!(para.get("baz"), Some("nee"));
            assert_eq!(para.to_string(), "bar: new\nbaz: nee\n");
        }

        #[test]
        fn test_update_remove() {
            let mut para: crate::Paragraph = "bar: bar\n# comment\nbaz: blah\n".parse().unwrap();

            #[derive(FromDeb822, ToDeb822)]
            struct Foo {
                bar: Option<String>,
                baz: String,
            }

            let mut foo: Foo = Foo::from_paragraph(&para).unwrap();
            assert_eq!(foo.bar, Some("bar".to_string()));
            assert_eq!(foo.baz, "blah");

            foo.bar = None;

            foo.update_paragraph(&mut para);

            assert_eq!(para.get("bar"), None);
            assert_eq!(para.get("baz"), Some("blah"));
            assert_eq!(para.to_string(), "baz: blah\n");
        }

        #[test]
        fn test_hashmap_prefix() {
            use std::collections::HashMap;

            #[derive(FromDeb822, ToDeb822)]
            struct VcsInfo {
                #[deb822(field = "Package")]
                package: String,
                #[deb822(prefix = "Vcs-")]
                vcs: HashMap<String, String>,
            }

            let para: crate::Paragraph =
                "Package: foo\nVcs-Git: https://example.com/git\nVcs-Browser: https://example.com\n"
                    .parse()
                    .unwrap();

            let info: VcsInfo = VcsInfo::from_paragraph(&para).unwrap();
            assert_eq!(info.package, "foo");
            assert_eq!(info.vcs.len(), 2);
            assert_eq!(
                info.vcs.get("Git"),
                Some(&"https://example.com/git".to_string())
            );
            assert_eq!(
                info.vcs.get("Browser"),
                Some(&"https://example.com".to_string())
            );

            // Test serialization
            let para2: crate::Paragraph = info.to_paragraph();
            assert_eq!(para2.get("Package"), Some("foo"));
            assert_eq!(para2.get("Vcs-Git"), Some("https://example.com/git"));
            assert_eq!(para2.get("Vcs-Browser"), Some("https://example.com"));
        }

        #[test]
        fn test_hashmap_prefix_optional() {
            use std::collections::HashMap;

            #[derive(FromDeb822, ToDeb822)]
            struct VcsInfo {
                #[deb822(field = "Package")]
                package: String,
                #[deb822(prefix = "Vcs-")]
                vcs: Option<HashMap<String, String>>,
            }

            // Test with VCS fields present
            let para: crate::Paragraph = "Package: foo\nVcs-Git: https://example.com/git\n"
                .parse()
                .unwrap();

            let info: VcsInfo = VcsInfo::from_paragraph(&para).unwrap();
            assert_eq!(info.package, "foo");
            assert!(info.vcs.is_some());
            assert_eq!(
                info.vcs.as_ref().unwrap().get("Git"),
                Some(&"https://example.com/git".to_string())
            );

            // Test without VCS fields
            let para2: crate::Paragraph = "Package: bar\n".parse().unwrap();
            let info2: VcsInfo = VcsInfo::from_paragraph(&para2).unwrap();
            assert_eq!(info2.package, "bar");
            assert!(info2.vcs.is_none());

            // Test serialization with None
            let para3: crate::Paragraph = info2.to_paragraph();
            assert_eq!(para3.get("Package"), Some("bar"));
            assert_eq!(para3.get("Vcs-Git"), None);
        }

        #[test]
        fn test_hashmap_prefix_empty() {
            use std::collections::HashMap;

            #[derive(FromDeb822, ToDeb822)]
            struct VcsInfo {
                #[deb822(field = "Package")]
                package: String,
                #[deb822(prefix = "Vcs-")]
                vcs: HashMap<String, String>,
            }

            let para: crate::Paragraph = "Package: foo\n".parse().unwrap();

            let info: VcsInfo = VcsInfo::from_paragraph(&para).unwrap();
            assert_eq!(info.package, "foo");
            assert_eq!(info.vcs.len(), 0);
        }

        #[test]
        fn test_hashmap_precedence() {
            use std::collections::HashMap;

            #[derive(FromDeb822, ToDeb822)]
            struct Config {
                #[deb822(field = "Package")]
                package: String,
                #[deb822(field = "Vcs-Git")]
                vcs_git: String,
                #[deb822(prefix = "Vcs-")]
                vcs_others: HashMap<String, String>,
            }

            let para: crate::Paragraph =
                "Package: foo\nVcs-Git: https://example.com/git\nVcs-Browser: https://example.com\n"
                    .parse()
                    .unwrap();

            let config: Config = Config::from_paragraph(&para).unwrap();
            assert_eq!(config.package, "foo");
            // Exact match takes precedence
            assert_eq!(config.vcs_git, "https://example.com/git");
            // Vcs-Git should NOT be in the prefix HashMap
            assert_eq!(config.vcs_others.get("Git"), None);
            // But Vcs-Browser should be
            assert_eq!(
                config.vcs_others.get("Browser"),
                Some(&"https://example.com".to_string())
            );
        }

        #[test]
        fn test_hashmap_longest_prefix() {
            use std::collections::HashMap;

            #[derive(FromDeb822, ToDeb822)]
            struct Config {
                #[deb822(field = "Package")]
                package: String,
                #[deb822(prefix = "Vcs-Git-")]
                vcs_git_fields: HashMap<String, String>,
                #[deb822(prefix = "Vcs-")]
                vcs_others: HashMap<String, String>,
            }

            let para: crate::Paragraph =
                "Package: foo\nVcs-Git-Url: https://example.com/git\nVcs-Git-Branch: main\nVcs-Browser: https://example.com\n"
                    .parse()
                    .unwrap();

            let config: Config = Config::from_paragraph(&para).unwrap();
            assert_eq!(config.package, "foo");
            // Longer prefix wins
            assert_eq!(
                config.vcs_git_fields.get("Url"),
                Some(&"https://example.com/git".to_string())
            );
            assert_eq!(
                config.vcs_git_fields.get("Branch"),
                Some(&"main".to_string())
            );
            // These should NOT be in the shorter prefix HashMap
            assert_eq!(config.vcs_others.get("Git-Url"), None);
            assert_eq!(config.vcs_others.get("Git-Branch"), None);
            // But Vcs-Browser should be in the shorter prefix HashMap
            assert_eq!(
                config.vcs_others.get("Browser"),
                Some(&"https://example.com".to_string())
            );
        }

        #[test]
        fn test_hashmap_catchall() {
            use std::collections::HashMap;

            #[derive(FromDeb822, ToDeb822)]
            struct Config {
                #[deb822(field = "Package")]
                package: String,
                #[deb822(prefix = "Vcs-")]
                vcs: HashMap<String, String>,
                // Catch-all for everything else
                other_fields: HashMap<String, String>,
            }

            let para: crate::Paragraph =
                "Package: foo\nVcs-Git: https://example.com/git\nMaintainer: John Doe\nSection: utils\n"
                    .parse()
                    .unwrap();

            let config: Config = Config::from_paragraph(&para).unwrap();
            assert_eq!(config.package, "foo");
            assert_eq!(
                config.vcs.get("Git"),
                Some(&"https://example.com/git".to_string())
            );
            // Catch-all gets fields not explicitly handled or prefix-matched
            assert_eq!(config.other_fields.get("Package"), None);
            assert_eq!(config.other_fields.get("Vcs-Git"), None);
            assert_eq!(
                config.other_fields.get("Maintainer"),
                Some(&"John Doe".to_string())
            );
            assert_eq!(
                config.other_fields.get("Section"),
                Some(&"utils".to_string())
            );
        }
    }
}
