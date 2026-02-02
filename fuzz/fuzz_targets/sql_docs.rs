#![no_main]

use libfuzzer_sys::fuzz_target;
use std::str;

use sql_docs::SqlDoc;

fuzz_target!(|data: String| {
    const SEPS: [&str; 6] = ["", " ", "\n", " | ", " • ", "--"];
    let _ = SqlDoc::builder_from_str(&data).build();
    let _ = SqlDoc::builder_from_str(&data).deny(&data).build();
    let _ = SqlDoc::builder_from_str(&data)
        .deny("nonexistent.sql")
        .deny("a.sql")
        .deny("b.sql")
        .build();
    let _ = SqlDoc::builder_from_str(&data).preserve_multiline().build();
    let _ = SqlDoc::builder_from_str(&data).flatten_multiline().build();
    for sep in SEPS {
        let _ = SqlDoc::builder_from_str(&data).flatten_multiline_with(sep).build();
    }
    let _ = SqlDoc::builder_from_str(&data).collect_single_nearest().build();
    let _ = SqlDoc::builder_from_str(&data).collect_all_leading().build();
    let _ = SqlDoc::builder_from_str(&data).collect_all_single_one_multi().build();
    for sep in SEPS {
        let _ = SqlDoc::builder_from_str(&data)
            .collect_single_nearest()
            .flatten_multiline_with(sep)
            .deny("nonexistent.sql")
            .build();

        let _ = SqlDoc::builder_from_str(&data)
            .collect_all_leading()
            .flatten_multiline_with(sep)
            .deny(&data)
            .build();

        let _ = SqlDoc::builder_from_str(&data)
            .collect_all_single_one_multi()
            .flatten_multiline_with(sep)
            .deny("a.sql")
            .deny("b.sql")
            .build();
    }
    let _ = SqlDoc::builder_from_str(&data)
        .collect_single_nearest()
        .collect_all_leading()
        .collect_all_single_one_multi()
        .build();

    let _ = SqlDoc::builder_from_str(&data)
        .flatten_multiline()
        .preserve_multiline()
        .flatten_multiline_with(" | ")
        .build();
});
