#![no_main]

use libfuzzer_sys::fuzz_target;
use std::str;

use sql_docs::SqlDoc;

fuzz_target!(|data: String| {
    
});