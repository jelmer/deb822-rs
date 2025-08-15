#![no_main]

use libfuzzer_sys::fuzz_target;
use deb822_fast::Deb822;
use std::str::FromStr;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        if let Ok(mut deb822) = Deb822::from_str(s) {
            // Test modification operations
            for paragraph in deb822.iter_mut() {
                // Try to set and remove fields
                paragraph.set("Test-Field", "test-value");
                paragraph.remove("Test-Field");
                paragraph.insert("Another-Field", "another-value");
                
                // Try to get all fields
                for (name, value) in paragraph.iter() {
                    let _ = (name, value);
                }
            }
            
            // Test serialization back to string
            let _ = deb822.to_string();
        }
    }
});