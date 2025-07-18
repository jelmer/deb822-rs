use dep3::lossy::PatchHeader;
use std::str::FromStr;

pub const TEXT: &str = r#"From: John Doe <john.doe@example>
Date: Mon, 1 Jan 2000 00:00:00 +0000
Subject: [PATCH] fix a bug
Bug-Debian: https://bugs.debian.org/123456
Bug: https://bugzilla.example.com/bug.cgi?id=123456
Forwarded: not-needed
"#;

pub fn main() {
    let patch_header = match PatchHeader::from_str(TEXT) {
        Ok(header) => header,
        Err(e) => {
            eprintln!("Failed to parse patch header: {}", e);
            std::process::exit(1);
        }
    };

    if let Some(description) = &patch_header.description {
        println!("Description: {}", description);
    }

    if let Some(bug_debian) = &patch_header.bug_debian {
        println!("Debian Bugs: {}", bug_debian);
    }
}
