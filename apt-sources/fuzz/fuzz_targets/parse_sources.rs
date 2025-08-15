#![no_main]

use libfuzzer_sys::fuzz_target;
use apt_sources::{Repositories, RepositoryType};
use std::str::FromStr;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // Fuzz repositories parser
        let _ = Repositories::from_str(s);
        
        // Fuzz repository type parser
        let _ = RepositoryType::from_str(s);
    }
});
