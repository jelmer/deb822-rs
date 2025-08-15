#![no_main]

use libfuzzer_sys::fuzz_target;
use deb822_lossless::Deb822;
use std::str::FromStr;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        if let Ok(mut deb822) = Deb822::from_str(s) {
            // Test adding paragraphs
            let _new_para = deb822.add_paragraph();
            
            // Test getting paragraphs (read-only iteration)
            for paragraph in deb822.paragraphs() {
                // Try to get some basic info from each paragraph
                let _ = paragraph.to_string();
                if let Some(first_key) = paragraph.keys().next() {
                    let _ = paragraph.get(&first_key);
                }
            }
            
            // Test serialization back to string
            let _ = deb822.to_string();
        }
    }
});