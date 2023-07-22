#![no_main]
#![allow(unused_must_use)]

use libfuzzer_sys::arbitrary;
use libfuzzer_sys::fuzz_target;

use sokoban::{State, Tile};

#[derive(arbitrary::Arbitrary, Clone, Debug)]
struct FuzzData {
    container: Vec<Tile>,
    player: (usize, usize),
    targets: Vec<(usize, usize)>,
    dim_r: usize,
    dim_c: usize,
}

fuzz_target!(|data: FuzzData| {
    State::new(
        data.container,
        data.player,
        data.targets,
        data.dim_r,
        data.dim_c,
    );
});
