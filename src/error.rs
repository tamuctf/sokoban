use crate::{Block, Direction, State};
use std::io::Error;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum SokobanError {
    #[error("Invalid bounds; got a raw Vec of length {}, but should have been {} ({} * {}).", .found, .r *.c, .r, .c)]
    InvalidBounds { found: usize, r: usize, c: usize },
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
    #[error("Encountered invalid character while parsing: {} ({:?})", .found, .pos)]
    InvalidChar { found: char, pos: (usize, usize) },
    #[error("Encountered inconsistent dimensions while parsing. Expected {}, found {}.", .expected, .found)]
    InconsistentDimensions { expected: usize, found: usize },
    #[error("Finished parsing, but found no player!")]
    MissingPlayer,
    #[error("Multiple players found in the map ({:?}, {:?})", .first, .second)]
    MultiplePlayers {
        first: (usize, usize),
        second: (usize, usize),
    },
    #[error("Encountered IO error while parsing: {}", .inner)]
    IOError { inner: std::io::Error },
}

impl From<std::io::Error> for SokobanError {
    fn from(inner: Error) -> Self {
        Self::IOError { inner }
    }
}

pub type SokobanResult<T> = Result<T, SokobanError>;
