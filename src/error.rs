use thiserror::Error;
use crate::{Block, Direction, State};

#[derive(Error, Debug, Clone)]
pub enum SokobanError {
    #[error("Invalid bounds; got a raw Vec of length {}, but should have been {} ({} * {}).", .found, .r *.c, .r, .c)]
    InvalidBounds {
        found: usize,
        r: usize,
        c: usize,
    },
    #[error("Invalid starting player position; found a {:?} where I needed a _ at ({}, {}).", .found, .r, .c)]
    InvalidStartingPosition {
        found: Option<Block>,
        r: usize,
        c: usize,
    },
    #[error("Invalid target position; found a {:?} where I needed a _ at ({}, {}).", .found, .r, .c)]
    InvalidTargetPosition {
        found: Option<Block>,
        r: usize,
        c: usize,
    },
    #[error("Invalid player move; couldn't move into the wall at ({}, {}).", .r, .c)]
    InvalidMoveWall {
        last_state: State,
        r: usize,
        c: usize,
    },
    #[error("Invalid player move; couldn't move into the block at ({}, {}) because the position behind it was blocked or out-of-bounds.", .r, .c)]
    InvalidMoveCrate {
        last_state: State,
        r: usize,
        c: usize,
    },
    #[error("Invalid player move; couldn't move {:?} out of bounds at ({}, {}).", .dir, .r, .c)]
    InvalidMoveOOB {
        last_state: State,
        dir: Direction,
        r: usize,
        c: usize,
    },
}

pub type SokobanResult<T> = Result<T, SokobanError>;
