#![no_main]

use libfuzzer_sys::fuzz_target;
use deb822_lossless::Deb822;
use std::str::FromStr;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let _ = Deb822::from_str(s);
    }
});
