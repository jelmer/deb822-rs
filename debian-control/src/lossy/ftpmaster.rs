//! FTP-master-related files

use deb822_fast::{FromDeb822Paragraph, FromDeb822, ToDeb822};

fn serialize_list(list: &Vec<String>) -> String {
    list.join("\n")
}

fn deserialize_list(list: &str) -> Result<Vec<String>, String> {
    Ok(list.lines().map(|s| s.to_string()).collect())
}

#[derive(Debug, Clone, FromDeb822, ToDeb822)]
/// A removal file
pub struct Removal {
    #[deb822(field = "Date")]
    /// The date of the removal
    pub date: String,

    #[deb822(field = "Suite")]
    /// The suite from which the package was removed
    pub suite: Option<String>,

    #[deb822(field = "Ftpmaster")]
    /// The FTP-master who performed the removal
    pub ftpmaster: String,

    #[deb822(field = "Sources", serialize_with = serialize_list, deserialize_with = deserialize_list)]
    /// The sources that were removed
    pub sources: Option<Vec<String>>,

    #[deb822(field = "Binaries", serialize_with = serialize_list, deserialize_with = deserialize_list)]
    /// The binaries that were removed
    pub binaries: Option<Vec<String>>,

    #[deb822(field = "Reason")]
    /// The reason for the removal
    pub reason: String,

    #[deb822(field = "Bug")]
    /// The bug number associated with the removal
    pub bug: Option<u32>,
}

impl std::str::FromStr for Removal {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let paragraph =
            deb822_fast::Paragraph::from_str(s).map_err(|e| e.to_string())?;
        Self::from_paragraph(&paragraph).map_err(|e| e.to_string())
    }
}
