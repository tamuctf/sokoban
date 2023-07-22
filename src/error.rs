//! Error types and utilities for sokoban puzzles.

use crate::{Direction, State, Tile};
use std::io::Error;
use thiserror::Error;

/// Primary error type for Sokoban, which enumerates all potential error conditions for a puzzle
/// and puzzle solution.
#[derive(Error, Debug)]
#[allow(clippy::module_name_repetitions)]
pub enum SokobanError {
    /// The puzzle generated or parsed had an invalid map backing for the provided dimensions. The
    /// map backing should always be `(number of rows)*(number of columns)` in length.
    #[error("Invalid bounds; got a raw Vec of length {}, but should have been {} ({} * {}).", .found, .r *.c, .r, .c)]
    InvalidBounds {
        /// The observed dimension of the provided map backing.
        found: usize,
        /// The number of rows specified.
        r: usize,
        /// The number of columns specified.
        c: usize,
    },
    /// The puzzle generated or parsed had a player at an invalid position, such as inside a wall,
    /// intersecting with a crate, or out of the bounds of the puzzle.
    #[error("Invalid starting player position; found a {:?} where I needed a _ at ({}, {}).", .found, .r, .c)]
    InvalidStartingPosition {
        /// The tile observed at the position, or `None` if the position specified was out of
        /// bounds.
        found: Option<Tile>,
        /// The row index specified in the starting position.
        r: usize,
        /// The column index specified in the starting position.
        c: usize,
    },
    /// The puzzle generated or parsed had a target at an invalid position, such as inside a wall or
    /// out of the bounds of the puzzle.
    #[error("Invalid target position; found a {:?} where I needed a _ at ({}, {}).", .found, .r, .c)]
    InvalidTargetPosition {
        /// The tile observed at the position, or `None` if the position specified was out of
        /// bounds.
        found: Option<Tile>,
        /// The row index specified for the target's position.
        r: usize,
        /// The column index specified for the target's position.
        c: usize,
    },
    /// The move, if applied to the current state, would have led to the player ending up in a wall.
    #[error("Invalid player move; couldn't move into the wall at ({}, {}).", .r, .c)]
    InvalidMoveWall {
        /// The state before the move, such that the user can recover and attempt another move.
        last_state: State,
        /// The row index the player attempted to move into.
        r: usize,
        /// The column index the player attempted to move into.
        c: usize,
        /// The direction in which the player attempted to move.
        dir: Direction,
    },
    /// The move, if applied to the current state, would have pushed a crate into an invalid
    /// position, such as into a wall, intersecting with another crate, or pushing the crate out of
    /// bounds.
    #[error("Invalid player move; couldn't move into the crate at ({}, {}) because the position behind it was blocked or out-of-bounds.", .r, .c)]
    InvalidMoveCrate {
        /// The state before the move, such that the user can recover and attempt another move.
        last_state: State,
        /// The row index of the crate that the player attempted to move.
        r: usize,
        /// The column index of the crate that the player attempted to move.
        c: usize,
        /// The direction in which the player attempted to move the crate.
        dir: Direction,
    },
    /// The move, if applied to the current state, would have led to the player ending up in a wall.
    /// Note that, unlike other move error variants, this returns the starting position of the
    /// player as some moves would result in unrepresentable positions.
    #[error("Invalid player move; couldn't move {:?} out of bounds at ({}, {}).", .dir, .r, .c)]
    InvalidMoveOOB {
        /// The state before the move, such that the user can recover and attempt another move.
        last_state: State,
        /// The row index the player started from.
        r: usize,
        /// The column index the player started from.
        c: usize,
        /// The direction in which the player attempted to move.
        dir: Direction,
    },
    /// During parsing, we encountered an invalid character.
    #[error("Encountered invalid character while parsing: {} ({}, {})", .found, .r, .c)]
    InvalidChar {
        /// The character that was discovered.
        found: char,
        /// The row index on the map associated with the invalid character.
        r: usize,
        /// The column index on the map associated with the invalid character.
        c: usize,
    },
    /// During parsing, we encountered a line which had a length that was greater or less than the
    /// number of columns expected for the currently parsed map.
    #[error("Encountered inconsistent dimensions while parsing. Expected {}, found {}.", .expected, .found)]
    InconsistentDimensions {
        /// The line number associated with the error.
        line: usize,
        /// The expected number of columns for the map, as derived from the first non-empty line.
        expected: usize,
        /// The number of columns found in this line.
        found: usize,
    },
    /// After parsing, we found that there was no player for the provided map.
    #[error("Finished parsing, but found no player!")]
    MissingPlayer,
    /// During parsing, multiple players were found.
    #[error("Multiple players found in the map ({:?}, {:?})", .first, .second)]
    MultiplePlayers {
        /// The position of the first player in (r, c) coordinates.
        first: (usize, usize),
        /// The position of the second player in (r, c) coordinates.
        second: (usize, usize),
    },
    /// During parsing, an IO error occurred.
    #[error("Encountered IO error while parsing: {}", .inner)]
    IOError {
        /// The wrapped IO error.
        inner: std::io::Error,
    },
}

impl From<std::io::Error> for SokobanError {
    fn from(inner: Error) -> Self {
        Self::IOError { inner }
    }
}

/// Result type for sokoban puzzles and solutions.
pub type SokobanResult<T> = Result<T, SokobanError>;
