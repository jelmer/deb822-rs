#![no_main]

use libfuzzer_sys::fuzz_target;
use dep3::lossy::PatchHeader as LossyPatchHeader;
use std::str::FromStr;

#[cfg(feature = "lossless")]
use dep3::lossless::PatchHeader as LosslessPatchHeader;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        // Fuzz lossy patch header parser
        let _ = LossyPatchHeader::from_str(s);
        
        #[cfg(feature = "lossless")]
        {
            // Fuzz lossless patch header parser
            let _ = LosslessPatchHeader::from_str(s);
        }
    }
});
