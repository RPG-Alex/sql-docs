#![no_main]

use libfuzzer_sys::fuzz_target;
use std::str;

use sql_docs::SqlDoc;
use std::{fs, path::PathBuf};

fuzz_target!(|data: &[u8]| {
   const MAX_FILES: usize = 16;
    const MAX_FILE_BYTES: usize = 8 * 1024;

    if data.is_empty() {
        return;
    }

    let tmp = match tempfile::tempdir() {
        Ok(t) => t,
        Err(_) => return,
    };
    let root = tmp.path();

    let mut i = 0usize;
    let file_count = (data[i] as usize % MAX_FILES).max(1);
    i += 1;

    for file_idx in 0..file_count {
        if i >= data.len() {
            break;
        }

        let name_len = (data[i] as usize % 24).max(1);
        i += 1;
        if i + name_len > data.len() {
            break;
        }

        let name_bytes = &data[i..i + name_len];
        i += name_len;

        let mut name = String::with_capacity(name_len);
        for &b in name_bytes {
            name.push(match b {
                b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' => b as char,
                _ => '_',
            });
        }

        let ext = if (file_idx + data[0] as usize) % 3 == 0 { "sql" } else { "txt" };
        let path = root.join(format!("{name}_{file_idx}.{ext}"));

        if i + 2 > data.len() {
            break;
        }
        let raw_len = u16::from_le_bytes([data[i], data[i + 1]]) as usize;
        i += 2;

        let content_len = raw_len % MAX_FILE_BYTES;
        if i + content_len > data.len() {
            break;
        }

        let content = &data[i..i + content_len];
        i += content_len;

        let text = String::from_utf8_lossy(content);
        let _ = fs::write(&path, text.as_bytes());
    }
    let res = SqlDoc::from_dir(root).build();
    if let Err(e) = res {
        let _ = e.to_string();
        let _ = std::error::Error::source(&e);
    }
});