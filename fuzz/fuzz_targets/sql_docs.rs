#![no_main]

use libfuzzer_sys::fuzz_target;
use std::str;

use sql_docs::fuzz_entry;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = str::from_utf8(data) {
        fuzz_entry(s);
    }
});
