#![no_main]
#![allow(unused_must_use)]

use libfuzzer_sys::arbitrary;
use libfuzzer_sys::fuzz_target;

use sokoban::{Block, State};

#[derive(arbitrary::Arbitrary, Clone, Debug)]
struct FuzzData {
    container: Vec<Block>,
    player: (u8, u8),
    targets: Vec<(u8, u8)>,
    dim_r: u8,
    dim_c: u8,
}

fn do_fuzz(data: FuzzData) {
    let mut targets: Vec<_> = data
        .targets
        .into_iter()
        .map(|(r, c)| (r as usize, c as usize))
        .collect();
    targets.sort();
    targets.dedup();
    if let Ok(state) = State::new(
        data.container,
        (data.player.0 as usize, data.player.1 as usize),
        targets,
        data.dim_r.into(),
        data.dim_c.into(),
    ) {
        let unparsed = format!("{:?}", state);
        let parsed = State::parse(unparsed.as_bytes()).expect("Parsing failed!");
        assert_eq!(state, parsed);
    }
}

fuzz_target!(|data: FuzzData| { do_fuzz(data) });
