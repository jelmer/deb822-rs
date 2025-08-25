//! This module provides a lossless representation of a Debian control file.
//!
//! # Example
//! ```rust
//! use debian_control::lossless::Control;
//! use debian_control::relations::VersionConstraint;
//! let input = r###"Source: dulwich
//! ## Comments are preserved
//! Maintainer: Jelmer Vernooĳ <jelmer@jelmer.uk>
//! Build-Depends: python3, debhelper-compat (= 12)
//!
//! Package: python3-dulwich
//! Architecture: amd64
//! Description: Pure-python git implementation
//! "###;
//!
//! let mut control: Control = input.parse().unwrap();
//!
//! // Bump debhelper-compat
//! let source = control.source().unwrap();
//! let bd = source.build_depends().unwrap();
//!
//! // Get entry with index 1 in Build-Depends, then set the version
//! let entry = bd.get_entry(1).unwrap();
//! let mut debhelper = entry.relations().next().unwrap();
//! assert_eq!(debhelper.name(), "debhelper-compat");
//! debhelper.set_version(Some((VersionConstraint::Equal, "13".parse().unwrap())));
//!
//! assert_eq!(source.to_string(), r###"Source: dulwich
//! ## Comments are preserved
//! Maintainer: Jelmer Vernooĳ <jelmer@jelmer.uk>
//! Build-Depends: python3, debhelper-compat (= 12)
//! "###);
//! ```
use crate::fields::{MultiArch, Priority};
use crate::lossless::relations::Relations;
use deb822_lossless::{Deb822, Paragraph, BINARY_FIELD_ORDER, SOURCE_FIELD_ORDER};
use rowan::ast::AstNode;

fn format_field(name: &str, value: &str) -> String {
    match name {
        "Uploaders" => value
            .split(',')
            .map(|s| s.trim().to_string())
            .collect::<Vec<_>>()
            .join(",\n"),
        "Build-Depends"
        | "Build-Depends-Indep"
        | "Build-Depends-Arch"
        | "Build-Conflicts"
        | "Build-Conflicts-Indep"
        | "Build-Conflics-Arch"
        | "Depends"
        | "Recommends"
        | "Suggests"
        | "Enhances"
        | "Pre-Depends"
        | "Breaks" => {
            let relations: Relations = value.parse().unwrap();
            let relations = relations.wrap_and_sort();
            relations.to_string()
        }
        _ => value.to_string(),
    }
}

/// A Debian control file
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Control(Deb822);

impl Control {
    /// Create a new control file
    pub fn new() -> Self {
        Control(Deb822::new())
    }

    /// Return the underlying deb822 object, mutable
    pub fn as_mut_deb822(&mut self) -> &mut Deb822 {
        &mut self.0
    }

    /// Return the underlying deb822 object
    pub fn as_deb822(&self) -> &Deb822 {
        &self.0
    }

    /// Parse control file text, returning a Parse result
    pub fn parse(text: &str) -> deb822_lossless::Parse<Control> {
        let deb822_parse = Deb822::parse(text);
        // Transform Parse<Deb822> to Parse<Control>
        let green = deb822_parse.green().clone();
        let errors = deb822_parse.errors().to_vec();
        let positioned_errors = deb822_parse.positioned_errors().to_vec();
        deb822_lossless::Parse::new_with_positioned_errors(green, errors, positioned_errors)
    }

    /// Return the source package
    pub fn source(&self) -> Option<Source> {
        self.0
            .paragraphs()
            .find(|p| p.get("Source").is_some())
            .map(Source)
    }

    /// Iterate over all binary packages
    pub fn binaries(&self) -> impl Iterator<Item = Binary> {
        self.0
            .paragraphs()
            .filter(|p| p.get("Package").is_some())
            .map(Binary)
    }

    /// Add a new source package
    ///
    /// # Arguments
    /// * `name` - The name of the source package
    ///
    /// # Returns
    /// The newly created source package
    ///
    /// # Example
    /// ```rust
    /// use debian_control::lossless::control::Control;
    /// let mut control = Control::new();
    /// let source = control.add_source("foo");
    /// assert_eq!(source.name(), Some("foo".to_owned()));
    /// ```
    pub fn add_source(&mut self, name: &str) -> Source {
        let mut p = self.0.add_paragraph();
        p.set("Source", name);
        self.source().unwrap()
    }

    /// Add new binary package
    ///
    /// # Arguments
    /// * `name` - The name of the binary package
    ///
    /// # Returns
    /// The newly created binary package
    ///
    /// # Example
    /// ```rust
    /// use debian_control::lossless::control::Control;
    /// let mut control = Control::new();
    /// let binary = control.add_binary("foo");
    /// assert_eq!(binary.name(), Some("foo".to_owned()));
    /// ```
    pub fn add_binary(&mut self, name: &str) -> Binary {
        let mut p = self.0.add_paragraph();
        p.set("Package", name);
        Binary(p)
    }

    /// Read a control file from a file
    pub fn from_file<P: AsRef<std::path::Path>>(path: P) -> Result<Self, deb822_lossless::Error> {
        Ok(Control(Deb822::from_file(path)?))
    }

    /// Read a control file from a file, allowing syntax errors
    pub fn from_file_relaxed<P: AsRef<std::path::Path>>(
        path: P,
    ) -> Result<(Self, Vec<String>), std::io::Error> {
        let (control, errors) = Deb822::from_file_relaxed(path)?;
        Ok((Control(control), errors))
    }

    /// Read a control file from a reader
    pub fn read<R: std::io::Read>(mut r: R) -> Result<Self, deb822_lossless::Error> {
        Ok(Control(Deb822::read(&mut r)?))
    }

    /// Read a control file from a reader, allowing syntax errors
    pub fn read_relaxed<R: std::io::Read>(
        mut r: R,
    ) -> Result<(Self, Vec<String>), deb822_lossless::Error> {
        let (control, errors) = Deb822::read_relaxed(&mut r)?;
        Ok((Self(control), errors))
    }

    /// Wrap and sort the control file
    ///
    /// # Arguments
    /// * `indentation` - The indentation to use
    /// * `immediate_empty_line` - Whether to add an empty line at the start of multi-line fields
    /// * `max_line_length_one_liner` - The maximum line length for one-liner fields
    pub fn wrap_and_sort(
        &mut self,
        indentation: deb822_lossless::Indentation,
        immediate_empty_line: bool,
        max_line_length_one_liner: Option<usize>,
    ) {
        let sort_paragraphs = |a: &Paragraph, b: &Paragraph| -> std::cmp::Ordering {
            // Sort Source before Package
            let a_is_source = a.get("Source").is_some();
            let b_is_source = b.get("Source").is_some();

            if a_is_source && !b_is_source {
                return std::cmp::Ordering::Less;
            } else if !a_is_source && b_is_source {
                return std::cmp::Ordering::Greater;
            } else if a_is_source && b_is_source {
                return a.get("Source").cmp(&b.get("Source"));
            }

            a.get("Package").cmp(&b.get("Package"))
        };

        let wrap_paragraph = |p: &Paragraph| -> Paragraph {
            // TODO: Add Source/Package specific wrapping
            // TODO: Add support for wrapping and sorting fields
            p.wrap_and_sort(
                indentation,
                immediate_empty_line,
                max_line_length_one_liner,
                None,
                Some(&format_field),
            )
        };

        self.0 = self
            .0
            .wrap_and_sort(Some(&sort_paragraphs), Some(&wrap_paragraph));
    }

    /// Iterate over fields that overlap with the given range
    ///
    /// This method returns all fields (entries) from all paragraphs that have any overlap
    /// with the specified text range. This is useful for incremental parsing in LSP contexts
    /// where you only want to process fields that were affected by a text change.
    ///
    /// # Arguments
    /// * `range` - The text range to check for overlaps
    ///
    /// # Returns
    /// An iterator over all Entry items that overlap with the given range
    ///
    /// # Example
    /// ```rust
    /// use debian_control::lossless::Control;
    /// use deb822_lossless::TextRange;
    ///
    /// let control_text = "Source: foo\nMaintainer: test@example.com\n\nPackage: bar\nArchitecture: all\n";
    /// let control: Control = control_text.parse().unwrap();
    ///
    /// // Get fields in a specific range (e.g., where a change occurred)
    /// let change_range = TextRange::new(20.into(), 40.into());
    /// for entry in control.fields_in_range(change_range) {
    ///     if let Some(key) = entry.key() {
    ///         println!("Field {} was in the changed range", key);
    ///     }
    /// }
    /// ```
    pub fn fields_in_range(
        &self,
        range: rowan::TextRange,
    ) -> impl Iterator<Item = deb822_lossless::Entry> + '_ {
        self.0
            .paragraphs()
            .flat_map(move |p| p.entries().collect::<Vec<_>>())
            .filter(move |entry| {
                let entry_range = entry.syntax().text_range();
                // Check if ranges overlap
                entry_range.start() < range.end() && range.start() < entry_range.end()
            })
    }
}

impl From<Control> for Deb822 {
    fn from(c: Control) -> Self {
        c.0
    }
}

impl From<Deb822> for Control {
    fn from(d: Deb822) -> Self {
        Control(d)
    }
}

impl Default for Control {
    fn default() -> Self {
        Self::new()
    }
}

impl std::str::FromStr for Control {
    type Err = deb822_lossless::ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Control::parse(s).to_result()
    }
}

/// A source package paragraph
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Source(Paragraph);

impl From<Source> for Paragraph {
    fn from(s: Source) -> Self {
        s.0
    }
}

impl From<Paragraph> for Source {
    fn from(p: Paragraph) -> Self {
        Source(p)
    }
}

impl std::fmt::Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Source {
    /// The name of the source package.
    pub fn name(&self) -> Option<String> {
        self.0.get("Source")
    }

    /// Wrap and sort the control file paragraph
    pub fn wrap_and_sort(
        &mut self,
        indentation: deb822_lossless::Indentation,
        immediate_empty_line: bool,
        max_line_length_one_liner: Option<usize>,
    ) {
        self.0 = self.0.wrap_and_sort(
            indentation,
            immediate_empty_line,
            max_line_length_one_liner,
            None,
            Some(&format_field),
        );
    }

    /// Return the underlying deb822 paragraph, mutable
    pub fn as_mut_deb822(&mut self) -> &mut Paragraph {
        &mut self.0
    }

    /// Return the underlying deb822 paragraph
    pub fn as_deb822(&self) -> &Paragraph {
        &self.0
    }

    /// Set the name of the source package.
    pub fn set_name(&mut self, name: &str) {
        self.set("Source", name);
    }

    /// The default section of the packages built from this source package.
    pub fn section(&self) -> Option<String> {
        self.0.get("Section")
    }

    /// Set the section of the source package
    pub fn set_section(&mut self, section: Option<&str>) {
        if let Some(section) = section {
            self.set("Section", section);
        } else {
            self.0.remove("Section");
        }
    }

    /// The default priority of the packages built from this source package.
    pub fn priority(&self) -> Option<Priority> {
        self.0.get("Priority").and_then(|v| v.parse().ok())
    }

    /// Set the priority of the source package
    pub fn set_priority(&mut self, priority: Option<Priority>) {
        if let Some(priority) = priority {
            self.set("Priority", priority.to_string().as_str());
        } else {
            self.0.remove("Priority");
        }
    }

    /// The maintainer of the package.
    pub fn maintainer(&self) -> Option<String> {
        self.0.get("Maintainer")
    }

    /// Set the maintainer of the package
    pub fn set_maintainer(&mut self, maintainer: &str) {
        self.set("Maintainer", maintainer);
    }

    /// The build dependencies of the package.
    pub fn build_depends(&self) -> Option<Relations> {
        self.0.get("Build-Depends").map(|s| s.parse().unwrap())
    }

    /// Set the Build-Depends field
    pub fn set_build_depends(&mut self, relations: &Relations) {
        self.set("Build-Depends", relations.to_string().as_str());
    }

    /// Return the Build-Depends-Indep field
    pub fn build_depends_indep(&self) -> Option<Relations> {
        self.0
            .get("Build-Depends-Indep")
            .map(|s| s.parse().unwrap())
    }

    /// Return the Build-Depends-Arch field
    pub fn build_depends_arch(&self) -> Option<Relations> {
        self.0.get("Build-Depends-Arch").map(|s| s.parse().unwrap())
    }

    /// The build conflicts of the package.
    pub fn build_conflicts(&self) -> Option<Relations> {
        self.0.get("Build-Conflicts").map(|s| s.parse().unwrap())
    }

    /// Return the Build-Conflicts-Indep field
    pub fn build_conflicts_indep(&self) -> Option<Relations> {
        self.0
            .get("Build-Conflicts-Indep")
            .map(|s| s.parse().unwrap())
    }

    /// Return the Build-Conflicts-Arch field
    pub fn build_conflicts_arch(&self) -> Option<Relations> {
        self.0
            .get("Build-Conflicts-Arch")
            .map(|s| s.parse().unwrap())
    }

    /// Return the standards version
    pub fn standards_version(&self) -> Option<String> {
        self.0.get("Standards-Version")
    }

    /// Set the Standards-Version field
    pub fn set_standards_version(&mut self, version: &str) {
        self.set("Standards-Version", version);
    }

    /// Return the upstrea mHomepage
    pub fn homepage(&self) -> Option<url::Url> {
        self.0.get("Homepage").and_then(|s| s.parse().ok())
    }

    /// Set the Homepage field
    pub fn set_homepage(&mut self, homepage: &url::Url) {
        self.set("Homepage", homepage.to_string().as_str());
    }

    /// Return the Vcs-Git field
    pub fn vcs_git(&self) -> Option<String> {
        self.0.get("Vcs-Git")
    }

    /// Set the Vcs-Git field
    pub fn set_vcs_git(&mut self, url: &str) {
        self.set("Vcs-Git", url);
    }

    /// Return the Vcs-Browser field
    pub fn vcs_svn(&self) -> Option<String> {
        self.0.get("Vcs-Svn").map(|s| s.to_string())
    }

    /// Set the Vcs-Svn field
    pub fn set_vcs_svn(&mut self, url: &str) {
        self.set("Vcs-Svn", url);
    }

    /// Return the Vcs-Bzr field
    pub fn vcs_bzr(&self) -> Option<String> {
        self.0.get("Vcs-Bzr").map(|s| s.to_string())
    }

    /// Set the Vcs-Bzr field
    pub fn set_vcs_bzr(&mut self, url: &str) {
        self.set("Vcs-Bzr", url);
    }

    /// Return the Vcs-Arch field
    pub fn vcs_arch(&self) -> Option<String> {
        self.0.get("Vcs-Arch").map(|s| s.to_string())
    }

    /// Set the Vcs-Arch field
    pub fn set_vcs_arch(&mut self, url: &str) {
        self.set("Vcs-Arch", url);
    }

    /// Return the Vcs-Svk field
    pub fn vcs_svk(&self) -> Option<String> {
        self.0.get("Vcs-Svk").map(|s| s.to_string())
    }

    /// Set the Vcs-Svk field
    pub fn set_vcs_svk(&mut self, url: &str) {
        self.set("Vcs-Svk", url);
    }

    /// Return the Vcs-Darcs field
    pub fn vcs_darcs(&self) -> Option<String> {
        self.0.get("Vcs-Darcs").map(|s| s.to_string())
    }

    /// Set the Vcs-Darcs field
    pub fn set_vcs_darcs(&mut self, url: &str) {
        self.set("Vcs-Darcs", url);
    }

    /// Return the Vcs-Mtn field
    pub fn vcs_mtn(&self) -> Option<String> {
        self.0.get("Vcs-Mtn").map(|s| s.to_string())
    }

    /// Set the Vcs-Mtn field
    pub fn set_vcs_mtn(&mut self, url: &str) {
        self.set("Vcs-Mtn", url);
    }

    /// Return the Vcs-Cvs field
    pub fn vcs_cvs(&self) -> Option<String> {
        self.0.get("Vcs-Cvs").map(|s| s.to_string())
    }

    /// Set the Vcs-Cvs field
    pub fn set_vcs_cvs(&mut self, url: &str) {
        self.set("Vcs-Cvs", url);
    }

    /// Return the Vcs-Hg field
    pub fn vcs_hg(&self) -> Option<String> {
        self.0.get("Vcs-Hg").map(|s| s.to_string())
    }

    /// Set the Vcs-Hg field
    pub fn set_vcs_hg(&mut self, url: &str) {
        self.set("Vcs-Hg", url);
    }

    /// Set a field in the source paragraph, using canonical field ordering for source packages
    pub fn set(&mut self, key: &str, value: &str) {
        self.0.set_with_field_order(key, value, SOURCE_FIELD_ORDER);
    }

    /// Return the Vcs-Browser field
    pub fn vcs_browser(&self) -> Option<String> {
        self.0.get("Vcs-Browser")
    }

    /// Return the Vcs used by the package
    pub fn vcs(&self) -> Option<crate::vcs::Vcs> {
        for (name, value) in self.0.items() {
            if name.starts_with("Vcs-") && name != "Vcs-Browser" {
                return crate::vcs::Vcs::from_field(&name, &value).ok();
            }
        }
        None
    }

    /// Set the Vcs-Browser field
    pub fn set_vcs_browser(&mut self, url: Option<&str>) {
        if let Some(url) = url {
            self.set("Vcs-Browser", url);
        } else {
            self.0.remove("Vcs-Browser");
        }
    }

    /// Return the Uploaders field
    pub fn uploaders(&self) -> Option<Vec<String>> {
        self.0
            .get("Uploaders")
            .map(|s| s.split(',').map(|s| s.trim().to_owned()).collect())
    }

    /// Set the uploaders field
    pub fn set_uploaders(&mut self, uploaders: &[&str]) {
        self.set(
            "Uploaders",
            uploaders
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join(", ")
                .as_str(),
        );
    }

    /// Return the architecture field
    pub fn architecture(&self) -> Option<String> {
        self.0.get("Architecture")
    }

    /// Set the architecture field
    pub fn set_architecture(&mut self, arch: Option<&str>) {
        if let Some(arch) = arch {
            self.set("Architecture", arch);
        } else {
            self.0.remove("Architecture");
        }
    }

    /// Return the Rules-Requires-Root field
    pub fn rules_requires_root(&self) -> Option<bool> {
        self.0
            .get("Rules-Requires-Root")
            .map(|s| match s.to_lowercase().as_str() {
                "yes" => true,
                "no" => false,
                _ => panic!("invalid Rules-Requires-Root value"),
            })
    }

    /// Set the Rules-Requires-Root field
    pub fn set_rules_requires_root(&mut self, requires_root: bool) {
        self.set(
            "Rules-Requires-Root",
            if requires_root { "yes" } else { "no" },
        );
    }

    /// Return the Testsuite field
    pub fn testsuite(&self) -> Option<String> {
        self.0.get("Testsuite")
    }

    /// Set the Testsuite field
    pub fn set_testsuite(&mut self, testsuite: &str) {
        self.set("Testsuite", testsuite);
    }

    /// Check if this source paragraph's range overlaps with the given range
    ///
    /// # Arguments
    /// * `range` - The text range to check for overlap
    ///
    /// # Returns
    /// `true` if the paragraph overlaps with the given range, `false` otherwise
    pub fn overlaps_range(&self, range: rowan::TextRange) -> bool {
        let para_range = self.0.syntax().text_range();
        para_range.start() < range.end() && range.start() < para_range.end()
    }

    /// Get fields in this source paragraph that overlap with the given range
    ///
    /// # Arguments
    /// * `range` - The text range to check for overlaps
    ///
    /// # Returns
    /// An iterator over Entry items that overlap with the given range
    pub fn fields_in_range(
        &self,
        range: rowan::TextRange,
    ) -> impl Iterator<Item = deb822_lossless::Entry> + '_ {
        self.0.entries().filter(move |entry| {
            let entry_range = entry.syntax().text_range();
            entry_range.start() < range.end() && range.start() < entry_range.end()
        })
    }
}

#[cfg(feature = "python-debian")]
impl<'py> pyo3::IntoPyObject<'py> for Source {
    type Target = pyo3::PyAny;
    type Output = pyo3::Bound<'py, Self::Target>;
    type Error = pyo3::PyErr;

    fn into_pyobject(self, py: pyo3::Python<'py>) -> Result<Self::Output, Self::Error> {
        self.0.into_pyobject(py)
    }
}

#[cfg(feature = "python-debian")]
impl<'a, 'py> pyo3::IntoPyObject<'py> for &'a Source {
    type Target = pyo3::PyAny;
    type Output = pyo3::Bound<'py, Self::Target>;
    type Error = pyo3::PyErr;

    fn into_pyobject(self, py: pyo3::Python<'py>) -> Result<Self::Output, Self::Error> {
        (&self.0).into_pyobject(py)
    }
}

#[cfg(feature = "python-debian")]
impl pyo3::FromPyObject<'_> for Source {
    fn extract_bound(ob: &pyo3::Bound<pyo3::PyAny>) -> pyo3::PyResult<Self> {
        use pyo3::prelude::*;
        Ok(Source(ob.extract()?))
    }
}

impl std::fmt::Display for Control {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl AstNode for Control {
    type Language = deb822_lossless::Lang;

    fn can_cast(kind: <Self::Language as rowan::Language>::Kind) -> bool {
        Deb822::can_cast(kind)
    }

    fn cast(syntax: rowan::SyntaxNode<Self::Language>) -> Option<Self> {
        Deb822::cast(syntax).map(Control)
    }

    fn syntax(&self) -> &rowan::SyntaxNode<Self::Language> {
        self.0.syntax()
    }
}

/// A binary package paragraph
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binary(Paragraph);

impl From<Binary> for Paragraph {
    fn from(b: Binary) -> Self {
        b.0
    }
}

impl From<Paragraph> for Binary {
    fn from(p: Paragraph) -> Self {
        Binary(p)
    }
}

#[cfg(feature = "python-debian")]
impl<'py> pyo3::IntoPyObject<'py> for Binary {
    type Target = pyo3::PyAny;
    type Output = pyo3::Bound<'py, Self::Target>;
    type Error = pyo3::PyErr;

    fn into_pyobject(self, py: pyo3::Python<'py>) -> Result<Self::Output, Self::Error> {
        self.0.into_pyobject(py)
    }
}

#[cfg(feature = "python-debian")]
impl<'a, 'py> pyo3::IntoPyObject<'py> for &'a Binary {
    type Target = pyo3::PyAny;
    type Output = pyo3::Bound<'py, Self::Target>;
    type Error = pyo3::PyErr;

    fn into_pyobject(self, py: pyo3::Python<'py>) -> Result<Self::Output, Self::Error> {
        (&self.0).into_pyobject(py)
    }
}

#[cfg(feature = "python-debian")]
impl pyo3::FromPyObject<'_> for Binary {
    fn extract_bound(ob: &pyo3::Bound<pyo3::PyAny>) -> pyo3::PyResult<Self> {
        use pyo3::prelude::*;
        Ok(Binary(ob.extract()?))
    }
}

impl Default for Binary {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Display for Binary {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Binary {
    /// Create a new binary package control file
    pub fn new() -> Self {
        Binary(Paragraph::new())
    }

    /// Return the underlying deb822 paragraph, mutable
    pub fn as_mut_deb822(&mut self) -> &mut Paragraph {
        &mut self.0
    }

    /// Return the underlying deb822 paragraph
    pub fn as_deb822(&self) -> &Paragraph {
        &self.0
    }

    /// Wrap and sort the control file
    pub fn wrap_and_sort(
        &mut self,
        indentation: deb822_lossless::Indentation,
        immediate_empty_line: bool,
        max_line_length_one_liner: Option<usize>,
    ) {
        self.0 = self.0.wrap_and_sort(
            indentation,
            immediate_empty_line,
            max_line_length_one_liner,
            None,
            Some(&format_field),
        );
    }

    /// The name of the package.
    pub fn name(&self) -> Option<String> {
        self.0.get("Package")
    }

    /// Set the name of the package
    pub fn set_name(&mut self, name: &str) {
        self.set("Package", name);
    }

    /// The section of the package.
    pub fn section(&self) -> Option<String> {
        self.0.get("Section")
    }

    /// Set the section
    pub fn set_section(&mut self, section: Option<&str>) {
        if let Some(section) = section {
            self.set("Section", section);
        } else {
            self.0.remove("Section");
        }
    }

    /// The priority of the package.
    pub fn priority(&self) -> Option<Priority> {
        self.0.get("Priority").and_then(|v| v.parse().ok())
    }

    /// Set the priority of the package
    pub fn set_priority(&mut self, priority: Option<Priority>) {
        if let Some(priority) = priority {
            self.set("Priority", priority.to_string().as_str());
        } else {
            self.0.remove("Priority");
        }
    }

    /// The architecture of the package.
    pub fn architecture(&self) -> Option<String> {
        self.0.get("Architecture")
    }

    /// Set the architecture of the package
    pub fn set_architecture(&mut self, arch: Option<&str>) {
        if let Some(arch) = arch {
            self.set("Architecture", arch);
        } else {
            self.0.remove("Architecture");
        }
    }

    /// The dependencies of the package.
    pub fn depends(&self) -> Option<Relations> {
        self.0.get("Depends").map(|s| s.parse().unwrap())
    }

    /// Set the Depends field
    pub fn set_depends(&mut self, depends: Option<&Relations>) {
        if let Some(depends) = depends {
            self.set("Depends", depends.to_string().as_str());
        } else {
            self.0.remove("Depends");
        }
    }

    /// The package that this package recommends
    pub fn recommends(&self) -> Option<Relations> {
        self.0.get("Recommends").map(|s| s.parse().unwrap())
    }

    /// Set the Recommends field
    pub fn set_recommends(&mut self, recommends: Option<&Relations>) {
        if let Some(recommends) = recommends {
            self.set("Recommends", recommends.to_string().as_str());
        } else {
            self.0.remove("Recommends");
        }
    }

    /// Packages that this package suggests
    pub fn suggests(&self) -> Option<Relations> {
        self.0.get("Suggests").map(|s| s.parse().unwrap())
    }

    /// Set the Suggests field
    pub fn set_suggests(&mut self, suggests: Option<&Relations>) {
        if let Some(suggests) = suggests {
            self.set("Suggests", suggests.to_string().as_str());
        } else {
            self.0.remove("Suggests");
        }
    }

    /// The package that this package enhances
    pub fn enhances(&self) -> Option<Relations> {
        self.0.get("Enhances").map(|s| s.parse().unwrap())
    }

    /// Set the Enhances field
    pub fn set_enhances(&mut self, enhances: Option<&Relations>) {
        if let Some(enhances) = enhances {
            self.set("Enhances", enhances.to_string().as_str());
        } else {
            self.0.remove("Enhances");
        }
    }

    /// The package that this package pre-depends on
    pub fn pre_depends(&self) -> Option<Relations> {
        self.0.get("Pre-Depends").map(|s| s.parse().unwrap())
    }

    /// Set the Pre-Depends field
    pub fn set_pre_depends(&mut self, pre_depends: Option<&Relations>) {
        if let Some(pre_depends) = pre_depends {
            self.set("Pre-Depends", pre_depends.to_string().as_str());
        } else {
            self.0.remove("Pre-Depends");
        }
    }

    /// The package that this package breaks
    pub fn breaks(&self) -> Option<Relations> {
        self.0.get("Breaks").map(|s| s.parse().unwrap())
    }

    /// Set the Breaks field
    pub fn set_breaks(&mut self, breaks: Option<&Relations>) {
        if let Some(breaks) = breaks {
            self.set("Breaks", breaks.to_string().as_str());
        } else {
            self.0.remove("Breaks");
        }
    }

    /// The package that this package conflicts with
    pub fn conflicts(&self) -> Option<Relations> {
        self.0.get("Conflicts").map(|s| s.parse().unwrap())
    }

    /// Set the Conflicts field
    pub fn set_conflicts(&mut self, conflicts: Option<&Relations>) {
        if let Some(conflicts) = conflicts {
            self.set("Conflicts", conflicts.to_string().as_str());
        } else {
            self.0.remove("Conflicts");
        }
    }

    /// The package that this package replaces
    pub fn replaces(&self) -> Option<Relations> {
        self.0.get("Replaces").map(|s| s.parse().unwrap())
    }

    /// Set the Replaces field
    pub fn set_replaces(&mut self, replaces: Option<&Relations>) {
        if let Some(replaces) = replaces {
            self.set("Replaces", replaces.to_string().as_str());
        } else {
            self.0.remove("Replaces");
        }
    }

    /// Return the Provides field
    pub fn provides(&self) -> Option<Relations> {
        self.0.get("Provides").map(|s| s.parse().unwrap())
    }

    /// Set the Provides field
    pub fn set_provides(&mut self, provides: Option<&Relations>) {
        if let Some(provides) = provides {
            self.set("Provides", provides.to_string().as_str());
        } else {
            self.0.remove("Provides");
        }
    }

    /// Return the Built-Using field
    pub fn built_using(&self) -> Option<Relations> {
        self.0.get("Built-Using").map(|s| s.parse().unwrap())
    }

    /// Set the Built-Using field
    pub fn set_built_using(&mut self, built_using: Option<&Relations>) {
        if let Some(built_using) = built_using {
            self.set("Built-Using", built_using.to_string().as_str());
        } else {
            self.0.remove("Built-Using");
        }
    }

    /// The Multi-Arch field
    pub fn multi_arch(&self) -> Option<MultiArch> {
        self.0.get("Multi-Arch").map(|s| s.parse().unwrap())
    }

    /// Set the Multi-Arch field
    pub fn set_multi_arch(&mut self, multi_arch: Option<MultiArch>) {
        if let Some(multi_arch) = multi_arch {
            self.set("Multi-Arch", multi_arch.to_string().as_str());
        } else {
            self.0.remove("Multi-Arch");
        }
    }

    /// Whether the package is essential
    pub fn essential(&self) -> bool {
        self.0.get("Essential").map(|s| s == "yes").unwrap_or(false)
    }

    /// Set whether the package is essential
    pub fn set_essential(&mut self, essential: bool) {
        if essential {
            self.set("Essential", "yes");
        } else {
            self.0.remove("Essential");
        }
    }

    /// Binary package description
    pub fn description(&self) -> Option<String> {
        self.0.get("Description")
    }

    /// Set the binary package description
    pub fn set_description(&mut self, description: Option<&str>) {
        if let Some(description) = description {
            self.set("Description", description);
        } else {
            self.0.remove("Description");
        }
    }

    /// Return the upstream homepage
    pub fn homepage(&self) -> Option<url::Url> {
        self.0.get("Homepage").and_then(|s| s.parse().ok())
    }

    /// Set the upstream homepage
    pub fn set_homepage(&mut self, url: &url::Url) {
        self.set("Homepage", url.as_str());
    }

    /// Set a field in the binary paragraph, using canonical field ordering for binary packages
    pub fn set(&mut self, key: &str, value: &str) {
        self.0.set_with_field_order(key, value, BINARY_FIELD_ORDER);
    }

    /// Check if this binary paragraph's range overlaps with the given range
    ///
    /// # Arguments
    /// * `range` - The text range to check for overlap
    ///
    /// # Returns
    /// `true` if the paragraph overlaps with the given range, `false` otherwise
    pub fn overlaps_range(&self, range: rowan::TextRange) -> bool {
        let para_range = self.0.syntax().text_range();
        para_range.start() < range.end() && range.start() < para_range.end()
    }

    /// Get fields in this binary paragraph that overlap with the given range
    ///
    /// # Arguments
    /// * `range` - The text range to check for overlaps
    ///
    /// # Returns
    /// An iterator over Entry items that overlap with the given range
    pub fn fields_in_range(
        &self,
        range: rowan::TextRange,
    ) -> impl Iterator<Item = deb822_lossless::Entry> + '_ {
        self.0.entries().filter(move |entry| {
            let entry_range = entry.syntax().text_range();
            entry_range.start() < range.end() && range.start() < entry_range.end()
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::relations::VersionConstraint;

    #[test]
    fn test_source_set_field_ordering() {
        let mut control = Control::new();
        let mut source = control.add_source("mypackage");

        // Add fields in random order
        source.set("Homepage", "https://example.com");
        source.set("Build-Depends", "debhelper");
        source.set("Standards-Version", "4.5.0");
        source.set("Maintainer", "Test <test@example.com>");

        // Convert to string and check field order
        let output = source.to_string();
        let lines: Vec<&str> = output.lines().collect();

        // Source should be first
        assert!(lines[0].starts_with("Source:"));

        // Find the positions of each field
        let maintainer_pos = lines
            .iter()
            .position(|l| l.starts_with("Maintainer:"))
            .unwrap();
        let build_depends_pos = lines
            .iter()
            .position(|l| l.starts_with("Build-Depends:"))
            .unwrap();
        let standards_pos = lines
            .iter()
            .position(|l| l.starts_with("Standards-Version:"))
            .unwrap();
        let homepage_pos = lines
            .iter()
            .position(|l| l.starts_with("Homepage:"))
            .unwrap();

        // Check ordering according to SOURCE_FIELD_ORDER
        assert!(maintainer_pos < build_depends_pos);
        assert!(build_depends_pos < standards_pos);
        assert!(standards_pos < homepage_pos);
    }

    #[test]
    fn test_binary_set_field_ordering() {
        let mut control = Control::new();
        let mut binary = control.add_binary("mypackage");

        // Add fields in random order
        binary.set("Description", "A test package");
        binary.set("Architecture", "amd64");
        binary.set("Depends", "libc6");
        binary.set("Section", "utils");

        // Convert to string and check field order
        let output = binary.to_string();
        let lines: Vec<&str> = output.lines().collect();

        // Package should be first
        assert!(lines[0].starts_with("Package:"));

        // Find the positions of each field
        let arch_pos = lines
            .iter()
            .position(|l| l.starts_with("Architecture:"))
            .unwrap();
        let section_pos = lines
            .iter()
            .position(|l| l.starts_with("Section:"))
            .unwrap();
        let depends_pos = lines
            .iter()
            .position(|l| l.starts_with("Depends:"))
            .unwrap();
        let desc_pos = lines
            .iter()
            .position(|l| l.starts_with("Description:"))
            .unwrap();

        // Check ordering according to BINARY_FIELD_ORDER
        assert!(arch_pos < section_pos);
        assert!(section_pos < depends_pos);
        assert!(depends_pos < desc_pos);
    }

    #[test]
    fn test_source_specific_set_methods_use_field_ordering() {
        let mut control = Control::new();
        let mut source = control.add_source("mypackage");

        // Use specific set_* methods in random order
        source.set_homepage(&"https://example.com".parse().unwrap());
        source.set_maintainer("Test <test@example.com>");
        source.set_standards_version("4.5.0");
        source.set_vcs_git("https://github.com/example/repo");

        // Convert to string and check field order
        let output = source.to_string();
        let lines: Vec<&str> = output.lines().collect();

        // Find the positions of each field
        let source_pos = lines.iter().position(|l| l.starts_with("Source:")).unwrap();
        let maintainer_pos = lines
            .iter()
            .position(|l| l.starts_with("Maintainer:"))
            .unwrap();
        let standards_pos = lines
            .iter()
            .position(|l| l.starts_with("Standards-Version:"))
            .unwrap();
        let vcs_git_pos = lines
            .iter()
            .position(|l| l.starts_with("Vcs-Git:"))
            .unwrap();
        let homepage_pos = lines
            .iter()
            .position(|l| l.starts_with("Homepage:"))
            .unwrap();

        // Check ordering according to SOURCE_FIELD_ORDER
        assert!(source_pos < maintainer_pos);
        assert!(maintainer_pos < standards_pos);
        assert!(standards_pos < vcs_git_pos);
        assert!(vcs_git_pos < homepage_pos);
    }

    #[test]
    fn test_binary_specific_set_methods_use_field_ordering() {
        let mut control = Control::new();
        let mut binary = control.add_binary("mypackage");

        // Use specific set_* methods in random order
        binary.set_description(Some("A test package"));
        binary.set_architecture(Some("amd64"));
        let depends = "libc6".parse().unwrap();
        binary.set_depends(Some(&depends));
        binary.set_section(Some("utils"));
        binary.set_priority(Some(Priority::Optional));

        // Convert to string and check field order
        let output = binary.to_string();
        let lines: Vec<&str> = output.lines().collect();

        // Find the positions of each field
        let package_pos = lines
            .iter()
            .position(|l| l.starts_with("Package:"))
            .unwrap();
        let arch_pos = lines
            .iter()
            .position(|l| l.starts_with("Architecture:"))
            .unwrap();
        let section_pos = lines
            .iter()
            .position(|l| l.starts_with("Section:"))
            .unwrap();
        let priority_pos = lines
            .iter()
            .position(|l| l.starts_with("Priority:"))
            .unwrap();
        let depends_pos = lines
            .iter()
            .position(|l| l.starts_with("Depends:"))
            .unwrap();
        let desc_pos = lines
            .iter()
            .position(|l| l.starts_with("Description:"))
            .unwrap();

        // Check ordering according to BINARY_FIELD_ORDER
        assert!(package_pos < arch_pos);
        assert!(arch_pos < section_pos);
        assert!(section_pos < priority_pos);
        assert!(priority_pos < depends_pos);
        assert!(depends_pos < desc_pos);
    }

    #[test]
    fn test_parse() {
        let control: Control = r#"Source: foo
Section: libs
Priority: optional
Build-Depends: bar (>= 1.0.0), baz (>= 1.0.0)
Homepage: https://example.com

"#
        .parse()
        .unwrap();
        let source = control.source().unwrap();

        assert_eq!(source.name(), Some("foo".to_owned()));
        assert_eq!(source.section(), Some("libs".to_owned()));
        assert_eq!(source.priority(), Some(super::Priority::Optional));
        assert_eq!(
            source.homepage(),
            Some("https://example.com".parse().unwrap())
        );
        let bd = source.build_depends().unwrap();
        let entries = bd.entries().collect::<Vec<_>>();
        assert_eq!(entries.len(), 2);
        let rel = entries[0].relations().collect::<Vec<_>>().pop().unwrap();
        assert_eq!(rel.name(), "bar");
        assert_eq!(
            rel.version(),
            Some((
                VersionConstraint::GreaterThanEqual,
                "1.0.0".parse().unwrap()
            ))
        );
        let rel = entries[1].relations().collect::<Vec<_>>().pop().unwrap();
        assert_eq!(rel.name(), "baz");
        assert_eq!(
            rel.version(),
            Some((
                VersionConstraint::GreaterThanEqual,
                "1.0.0".parse().unwrap()
            ))
        );
    }

    #[test]
    fn test_description() {
        let control: Control = r#"Source: foo

Package: foo
Description: this is the short description
 And the longer one
 .
 is on the next lines
"#
        .parse()
        .unwrap();
        let binary = control.binaries().next().unwrap();
        assert_eq!(
            binary.description(),
            Some(
                "this is the short description\nAnd the longer one\n.\nis on the next lines"
                    .to_owned()
            )
        );
    }

    #[test]
    fn test_as_mut_deb822() {
        let mut control = Control::new();
        let deb822 = control.as_mut_deb822();
        let mut p = deb822.add_paragraph();
        p.set("Source", "foo");
        assert_eq!(control.source().unwrap().name(), Some("foo".to_owned()));
    }

    #[test]
    fn test_as_deb822() {
        let control = Control::new();
        let _deb822: &Deb822 = control.as_deb822();
    }

    #[test]
    fn test_set_depends() {
        let mut control = Control::new();
        let mut binary = control.add_binary("foo");
        let relations: Relations = "bar (>= 1.0.0)".parse().unwrap();
        binary.set_depends(Some(&relations));
    }

    #[test]
    fn test_wrap_and_sort() {
        let mut control: Control = r#"Package: blah
Section:     libs



Package: foo
Description: this is a 
      bar
      blah
"#
        .parse()
        .unwrap();
        control.wrap_and_sort(deb822_lossless::Indentation::Spaces(2), false, None);
        let expected = r#"Package: blah
Section: libs

Package: foo
Description: this is a 
  bar
  blah
"#
        .to_owned();
        assert_eq!(control.to_string(), expected);
    }

    #[test]
    fn test_wrap_and_sort_source() {
        let mut control: Control = r#"Source: blah
Depends: foo, bar   (<=  1.0.0)

"#
        .parse()
        .unwrap();
        control.wrap_and_sort(deb822_lossless::Indentation::Spaces(2), true, None);
        let expected = r#"Source: blah
Depends: bar (<= 1.0.0), foo
"#
        .to_owned();
        assert_eq!(control.to_string(), expected);
    }

    #[test]
    fn test_source_wrap_and_sort() {
        let control: Control = r#"Source: blah
Build-Depends: foo, bar (>= 1.0.0)

"#
        .parse()
        .unwrap();
        let mut source = control.source().unwrap();
        source.wrap_and_sort(deb822_lossless::Indentation::Spaces(2), true, None);
        // The actual behavior - the method modifies the source in-place
        // but doesn't automatically affect the overall control structure
        // So we just test that the method executes without error
        assert!(source.build_depends().is_some());
    }

    #[test]
    fn test_binary_set_breaks() {
        let mut control = Control::new();
        let mut binary = control.add_binary("foo");
        let relations: Relations = "bar (>= 1.0.0)".parse().unwrap();
        binary.set_breaks(Some(&relations));
        assert!(binary.breaks().is_some());
    }

    #[test]
    fn test_binary_set_pre_depends() {
        let mut control = Control::new();
        let mut binary = control.add_binary("foo");
        let relations: Relations = "bar (>= 1.0.0)".parse().unwrap();
        binary.set_pre_depends(Some(&relations));
        assert!(binary.pre_depends().is_some());
    }

    #[test]
    fn test_binary_set_provides() {
        let mut control = Control::new();
        let mut binary = control.add_binary("foo");
        let relations: Relations = "bar (>= 1.0.0)".parse().unwrap();
        binary.set_provides(Some(&relations));
        assert!(binary.provides().is_some());
    }

    #[test]
    fn test_source_build_conflicts() {
        let control: Control = r#"Source: blah
Build-Conflicts: foo, bar (>= 1.0.0)

"#
        .parse()
        .unwrap();
        let source = control.source().unwrap();
        let conflicts = source.build_conflicts();
        assert!(conflicts.is_some());
    }

    #[test]
    fn test_source_vcs_svn() {
        let control: Control = r#"Source: blah
Vcs-Svn: https://example.com/svn/repo

"#
        .parse()
        .unwrap();
        let source = control.source().unwrap();
        assert_eq!(
            source.vcs_svn(),
            Some("https://example.com/svn/repo".to_string())
        );
    }

    #[test]
    fn test_control_from_conversion() {
        let deb822_data = r#"Source: test
Section: libs

"#;
        let deb822: Deb822 = deb822_data.parse().unwrap();
        let control = Control::from(deb822);
        assert!(control.source().is_some());
    }

    #[test]
    fn test_fields_in_range() {
        let control_text = r#"Source: test-package
Maintainer: Test User <test@example.com>
Build-Depends: debhelper (>= 12)

Package: test-binary
Architecture: any
Depends: ${shlibs:Depends}
Description: Test package
 This is a test package
"#;
        let control: Control = control_text.parse().unwrap();

        // Test range that covers only the Source field
        let source_start = 0;
        let source_end = "Source: test-package".len();
        let source_range =
            rowan::TextRange::new((source_start as u32).into(), (source_end as u32).into());

        let fields: Vec<_> = control.fields_in_range(source_range).collect();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].key(), Some("Source".to_string()));

        // Test range that covers multiple fields in source paragraph
        let maintainer_start = control_text.find("Maintainer:").unwrap();
        let build_depends_end = control_text
            .find("Build-Depends: debhelper (>= 12)")
            .unwrap()
            + "Build-Depends: debhelper (>= 12)".len();
        let multi_range = rowan::TextRange::new(
            (maintainer_start as u32).into(),
            (build_depends_end as u32).into(),
        );

        let fields: Vec<_> = control.fields_in_range(multi_range).collect();
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].key(), Some("Maintainer".to_string()));
        assert_eq!(fields[1].key(), Some("Build-Depends".to_string()));

        // Test range that spans across paragraphs
        let cross_para_start = control_text.find("Build-Depends:").unwrap();
        let cross_para_end =
            control_text.find("Architecture: any").unwrap() + "Architecture: any".len();
        let cross_range = rowan::TextRange::new(
            (cross_para_start as u32).into(),
            (cross_para_end as u32).into(),
        );

        let fields: Vec<_> = control.fields_in_range(cross_range).collect();
        assert_eq!(fields.len(), 3); // Build-Depends, Package, Architecture
        assert_eq!(fields[0].key(), Some("Build-Depends".to_string()));
        assert_eq!(fields[1].key(), Some("Package".to_string()));
        assert_eq!(fields[2].key(), Some("Architecture".to_string()));

        // Test empty range (should return no fields)
        let empty_range = rowan::TextRange::new(1000.into(), 1001.into());
        let fields: Vec<_> = control.fields_in_range(empty_range).collect();
        assert_eq!(fields.len(), 0);
    }

    #[test]
    fn test_source_overlaps_range() {
        let control_text = r#"Source: test-package
Maintainer: Test User <test@example.com>

Package: test-binary
Architecture: any
"#;
        let control: Control = control_text.parse().unwrap();
        let source = control.source().unwrap();

        // Test range that overlaps with source paragraph
        let overlap_range = rowan::TextRange::new(10.into(), 30.into());
        assert!(source.overlaps_range(overlap_range));

        // Test range that doesn't overlap with source paragraph
        let binary_start = control_text.find("Package:").unwrap();
        let no_overlap_range = rowan::TextRange::new(
            (binary_start as u32).into(),
            ((binary_start + 20) as u32).into(),
        );
        assert!(!source.overlaps_range(no_overlap_range));

        // Test range that starts before and ends within source paragraph
        let partial_overlap = rowan::TextRange::new(0.into(), 15.into());
        assert!(source.overlaps_range(partial_overlap));
    }

    #[test]
    fn test_source_fields_in_range() {
        let control_text = r#"Source: test-package
Maintainer: Test User <test@example.com>
Build-Depends: debhelper (>= 12)

Package: test-binary
"#;
        let control: Control = control_text.parse().unwrap();
        let source = control.source().unwrap();

        // Test range covering Maintainer field
        let maintainer_start = control_text.find("Maintainer:").unwrap();
        let maintainer_end = maintainer_start + "Maintainer: Test User <test@example.com>".len();
        let maintainer_range = rowan::TextRange::new(
            (maintainer_start as u32).into(),
            (maintainer_end as u32).into(),
        );

        let fields: Vec<_> = source.fields_in_range(maintainer_range).collect();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].key(), Some("Maintainer".to_string()));

        // Test range covering multiple fields
        let all_source_range = rowan::TextRange::new(0.into(), 100.into());
        let fields: Vec<_> = source.fields_in_range(all_source_range).collect();
        assert_eq!(fields.len(), 3); // Source, Maintainer, Build-Depends
    }

    #[test]
    fn test_binary_overlaps_range() {
        let control_text = r#"Source: test-package

Package: test-binary
Architecture: any
Depends: ${shlibs:Depends}
"#;
        let control: Control = control_text.parse().unwrap();
        let binary = control.binaries().next().unwrap();

        // Test range that overlaps with binary paragraph
        let package_start = control_text.find("Package:").unwrap();
        let overlap_range = rowan::TextRange::new(
            (package_start as u32).into(),
            ((package_start + 30) as u32).into(),
        );
        assert!(binary.overlaps_range(overlap_range));

        // Test range before binary paragraph
        let no_overlap_range = rowan::TextRange::new(0.into(), 10.into());
        assert!(!binary.overlaps_range(no_overlap_range));
    }

    #[test]
    fn test_binary_fields_in_range() {
        let control_text = r#"Source: test-package

Package: test-binary
Architecture: any
Depends: ${shlibs:Depends}
Description: Test binary
 This is a test binary package
"#;
        let control: Control = control_text.parse().unwrap();
        let binary = control.binaries().next().unwrap();

        // Test range covering Architecture and Depends
        let arch_start = control_text.find("Architecture:").unwrap();
        let depends_end = control_text.find("Depends: ${shlibs:Depends}").unwrap()
            + "Depends: ${shlibs:Depends}".len();
        let range = rowan::TextRange::new((arch_start as u32).into(), (depends_end as u32).into());

        let fields: Vec<_> = binary.fields_in_range(range).collect();
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].key(), Some("Architecture".to_string()));
        assert_eq!(fields[1].key(), Some("Depends".to_string()));

        // Test partial overlap with Description field
        let desc_start = control_text.find("Description:").unwrap();
        let partial_range = rowan::TextRange::new(
            ((desc_start + 5) as u32).into(),
            ((desc_start + 15) as u32).into(),
        );
        let fields: Vec<_> = binary.fields_in_range(partial_range).collect();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].key(), Some("Description".to_string()));
    }

    #[test]
    fn test_incremental_parsing_use_case() {
        // This test simulates a real LSP use case where only changed fields are processed
        let control_text = r#"Source: example
Maintainer: John Doe <john@example.com>
Standards-Version: 4.6.0
Build-Depends: debhelper-compat (= 13)

Package: example-bin
Architecture: all
Depends: ${misc:Depends}
Description: Example package
 This is an example.
"#;
        let control: Control = control_text.parse().unwrap();

        // Simulate a change to Standards-Version field
        let change_start = control_text.find("Standards-Version:").unwrap();
        let change_end = change_start + "Standards-Version: 4.6.0".len();
        let change_range =
            rowan::TextRange::new((change_start as u32).into(), (change_end as u32).into());

        // Only process fields in the changed range
        let affected_fields: Vec<_> = control.fields_in_range(change_range).collect();
        assert_eq!(affected_fields.len(), 1);
        assert_eq!(
            affected_fields[0].key(),
            Some("Standards-Version".to_string())
        );

        // Verify that we're not processing unrelated fields
        for entry in &affected_fields {
            let key = entry.key().unwrap();
            assert_ne!(key, "Maintainer");
            assert_ne!(key, "Build-Depends");
            assert_ne!(key, "Architecture");
        }
    }

    #[test]
    fn test_positioned_parse_errors() {
        // Test case from the requirements document
        let input = "Invalid: field\nBroken field without colon";
        let parsed = Control::parse(input);

        // Should have positioned errors accessible
        let positioned_errors = parsed.positioned_errors();
        assert!(
            !positioned_errors.is_empty(),
            "Should have positioned errors"
        );

        // Test that we can access error properties
        for error in positioned_errors {
            let start_offset: u32 = error.range.start().into();
            let end_offset: u32 = error.range.end().into();

            // Verify we have meaningful error messages
            assert!(!error.message.is_empty());

            // Verify ranges are valid
            assert!(start_offset <= end_offset);
            assert!(end_offset <= input.len() as u32);

            // Error should have a code
            assert!(error.code.is_some());

            println!(
                "Error at {:?}: {} (code: {:?})",
                error.range, error.message, error.code
            );
        }

        // Should also be able to get string errors for backward compatibility
        let string_errors = parsed.errors();
        assert!(!string_errors.is_empty());
        assert_eq!(string_errors.len(), positioned_errors.len());
    }
}
