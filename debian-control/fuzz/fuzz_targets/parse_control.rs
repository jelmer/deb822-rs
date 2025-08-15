#![no_main]

use libfuzzer_sys::fuzz_target;
use debian_control::lossy::Control;
use debian_control::lossy::apt::{Package, Source};
use std::str::FromStr;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // Fuzz main control file parser
        let _ = Control::from_str(s);
        
        // Fuzz apt package parser
        let _ = Package::from_str(s);
        
        // Fuzz apt source parser
        let _ = Source::from_str(s);
    }
});
