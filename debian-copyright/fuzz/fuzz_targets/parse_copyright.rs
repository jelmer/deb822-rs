#![no_main]

use libfuzzer_sys::fuzz_target;
use debian_copyright::lossy::Copyright as LossyCopyright;
use debian_copyright::lossless::Copyright as LosslessCopyright;
use std::str::FromStr;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // Fuzz lossy copyright parser
        let _ = LossyCopyright::from_str(s);
        
        // Fuzz lossless copyright parser
        let _ = LosslessCopyright::from_str(s);
    }
});
