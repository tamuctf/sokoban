#![no_main]
#![allow(unused_must_use)]

use libfuzzer_sys::arbitrary;
use libfuzzer_sys::fuzz_target;

use sokoban::{Block, State};

#[derive(arbitrary::Arbitrary, Clone, Debug)]
struct FuzzData {
    container: Vec<Block>,
    player: (usize, usize),
    targets: Vec<(usize, usize)>,
    dim_r: usize,
    dim_c: usize,
}

fn do_fuzz(data: FuzzData) {
    if let Ok(state) = State::new(
        data.container,
        data.player,
        data.targets,
        data.dim_r,
        data.dim_c,
    ) {
        let unparsed = format!("{:?}", state);
        let parsed = State::parse(unparsed.as_bytes()).expect("Parsing failed!");
        assert_eq!(state, parsed);
    }
}

fuzz_target!(|data: FuzzData| { do_fuzz(data) });
