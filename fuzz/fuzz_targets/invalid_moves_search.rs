#![no_main]
#![allow(unused_must_use)]

use libfuzzer_sys::fuzz_target;
use libfuzzer_sys::arbitrary;

use sokoban::{Block, Direction, State};
use sokoban::error::SokobanResult;

fn generate_room_with_box() -> SokobanResult<State> {
    let rows = 20;
    let cols = 20;
    let mut raw = vec![Block::Wall; rows * cols];
    let player = (9, 9);
    for i in (21..(raw.len() - 20)).step_by(20) {
        raw.iter_mut().skip(i).take(18).for_each(|block| *block = Block::Floor);
    }
    raw[4 + 4 * 20] = Block::Crate;

    State::new(raw, player, vec![(10, 10)], rows, cols)
}

fuzz_target!(|data: Vec<Direction>| {
    if let Ok(valid) = data.iter().fold(generate_room_with_box(), |state, &dir| state.and_then(|state| state.move_player(dir))) {
        if valid.in_solution_state() {
            panic!("Found a solution: {:?}", data);
        }
    }
});
