#![no_main]

use libfuzzer_sys::fuzz_target;
use debian_control::lossy::{Relation, Relations};
use std::str::FromStr;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // Fuzz relation parsing
        let _ = Relation::from_str(s);
        let _ = Relations::from_str(s);
    }
});