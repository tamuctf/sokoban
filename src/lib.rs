//! `sokoban`, a general purpose crate for verifying sokoban puzzle states and solutions.

#![deny(clippy::all)]
#![deny(clippy::pedantic)]
#![deny(missing_docs)]

pub mod error;

use crate::error::{SokobanError, SokobanResult};
use std::fmt::{Debug, Formatter, Write};
use std::hash::{Hash, Hasher};
use std::io::BufRead;
use std::iter::Enumerate;
use std::ops::Index;

/// The individual tiles present on a sokoban map.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Tile {
    /// A crate tile represents a movable "crate" to be pushed by the player into the designated
    /// target positions.
    ///
    /// Crate tiles that are not located in target positions are denoted with
    /// `m` and crate tiles that are located in target positions are denoted with `M`.
    Crate,
    /// A floor tile represents a space in which players and crates may move or be moved.
    ///
    /// A floor tile is denoted with `_`. Floor tiles containing a target position are denoted with
    /// `.`. Floor tiles on which a player is currently located is denoted with `x`. Finally, a
    /// floor tile which contains both a player and a target position is denoted with `X`.
    Floor,
    /// A wall tile represents an inaccessible position into which neither players nor crates may
    /// move or be moved.
    ///
    /// A wall tile is denoted with `#`.
    Wall,
}

impl Debug for Tile {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Tile::Crate => f.write_char('m'),
            Tile::Floor => f.write_char('_'),
            Tile::Wall => f.write_char('#'),
        }
    }
}

impl TryFrom<char> for Tile {
    type Error = char;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'm' => Ok(Tile::Crate),
            '_' => Ok(Tile::Floor),
            '#' => Ok(Tile::Wall),
            c => Err(c),
        }
    }
}

/// A direction in which a player can move or move a crate.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Direction {
    /// The direction in which a move subtracts from the row index.
    Up,
    /// The direction in which a move adds to the row index.
    Down,
    /// The direction in which a move subtracts from the column index.
    Left,
    /// The direction in which a move adds to the column index.
    Right,
}

impl Direction {
    /// Indicates the position to which a player would move if starting in the provided position.
    ///
    /// This method returns `None` if the next position cannot be computed, such as in the case of
    /// integer under- or over-flow.
    #[must_use]
    pub fn go(&self, orig: (usize, usize)) -> Option<(usize, usize)> {
        let pos = match self {
            Direction::Up => {
                let Some(r) = orig.0.checked_sub(1) else { return None; };
                (r, orig.1)
            }
            Direction::Down => {
                let Some(r) = orig.0.checked_add(1) else { return None; };
                (r, orig.1)
            }
            Direction::Left => {
                let Some(c) = orig.1.checked_sub(1) else { return None; };
                (orig.0, c)
            }
            Direction::Right => {
                let Some(c) = orig.1.checked_add(1) else { return None; };
                (orig.0, c)
            }
        };
        Some(pos)
    }
}

impl Debug for Direction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Direction::Up => f.write_str("up"),
            Direction::Down => f.write_str("down"),
            Direction::Left => f.write_str("left"),
            Direction::Right => f.write_str("right"),
        }
    }
}

/// The state of the sokoban puzzle, including the map, player position, the targets of the puzzle,
/// and the number of moves so far.
#[derive(Clone, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct State {
    container: Vec<Tile>,
    player: (usize, usize),
    targets: Vec<(usize, usize)>,
    moves: usize,
    dim_r: usize,
    dim_c: usize,
}

impl State {
    fn checked_bounds(&self, pos: (usize, usize)) -> Option<usize> {
        if pos.0 < self.dim_r && pos.1 < self.dim_c {
            self.dim_c
                .checked_mul(pos.0)
                .and_then(|r| r.checked_add(pos.1))
        } else {
            None
        }
    }

    /// Create a new state by providing its raw components.
    ///
    /// The vector of tiles must be laid out such that the rows are contiguous. For each index `i`
    /// in the vector, with `r` as the number of rows and `c` as the number of columns, the current
    /// position of the map is `(i / c, i % c)`.
    ///
    /// # Errors
    ///
    /// The state is invalid in the following conditions:
    ///  - The vector's length is not `r*c`
    ///  - The player or any of the targets are not within the bounds
    ///  - The player or any of the targets are not on a floor tile
    ///
    /// See [`SokobanError`] for the relevant error variants.
    pub fn new(
        raw: Vec<Tile>,
        player: (usize, usize),
        targets: Vec<(usize, usize)>,
        dim_r: usize,
        dim_c: usize,
    ) -> SokobanResult<Self> {
        if dim_r.checked_mul(dim_c).is_none() {
            return Err(SokobanError::InvalidBounds {
                found: usize::MAX,
                r: dim_r,
                c: dim_c,
            });
        }
        if raw.len() != dim_r * dim_c {
            return Err(SokobanError::InvalidBounds {
                found: raw.len(),
                r: dim_r,
                c: dim_c,
            });
        }
        let state = State {
            container: raw,
            player,
            targets,
            moves: 0,
            dim_r,
            dim_c,
        };
        let checked_player = state.checked_bounds(state.player);
        if let Some(checked_player) = checked_player {
            let tile = state.container[checked_player];
            if tile != Tile::Floor {
                return Err(SokobanError::InvalidStartingPosition {
                    found: Some(tile),
                    r: player.0,
                    c: player.1,
                });
            }
        } else {
            return Err(SokobanError::InvalidStartingPosition {
                found: None,
                r: player.0,
                c: player.1,
            });
        }
        for target in state.targets.iter().copied() {
            let checked_target = state.checked_bounds(target);
            if let Some(checked_target) = checked_target {
                let tile = state.container[checked_target];
                if tile == Tile::Wall {
                    return Err(SokobanError::InvalidTargetPosition {
                        found: Some(tile),
                        r: target.0,
                        c: target.1,
                    });
                }
            } else {
                return Err(SokobanError::InvalidTargetPosition {
                    found: None,
                    r: target.0,
                    c: target.1,
                });
            }
        }
        Ok(state)
    }

    /// Determines if this state represents a solved state, i.e. all the crates have been moved to
    /// the target position.
    ///
    /// A state with no targets is always in a solution state.
    #[must_use]
    pub fn in_solution_state(&self) -> bool {
        self.targets
            .iter()
            .copied()
            .all(|target| self[target] == Tile::Crate)
    }

    /// Consumes this state and returns a new state with the player in the specified position.
    ///
    /// # Errors
    ///
    /// This method errors in the following conditions:
    ///  - The player would move out of bounds or into a wall
    ///  - The player pushes a crate that would move:
    ///     - out of bounds
    ///     - into a wall
    ///     - into another crate
    pub fn move_player(mut self, direction: Direction) -> SokobanResult<Self> {
        let next_pos = direction.go(self.player);
        if let Some(next_pos) = next_pos {
            if let Some(npi) = self.checked_bounds(next_pos) {
                #[allow(clippy::match_on_vec_items)]
                match self.container[npi] {
                    Tile::Crate => {
                        let cnpi = direction
                            .go(next_pos)
                            .and_then(|c_next_pos| self.checked_bounds(c_next_pos));
                        if let Some(cnpi) = cnpi {
                            if self.container[cnpi] != Tile::Floor {
                                return Err(SokobanError::InvalidMoveCrate {
                                    last_state: self,
                                    r: next_pos.0,
                                    c: next_pos.1,
                                    dir: direction,
                                });
                            }
                            self.container[npi] = Tile::Floor;
                            self.container[cnpi] = Tile::Crate;
                        } else {
                            return Err(SokobanError::InvalidMoveCrate {
                                last_state: self,
                                r: next_pos.0,
                                c: next_pos.1,
                                dir: direction,
                            });
                        }
                    }
                    Tile::Wall => {
                        return Err(SokobanError::InvalidMoveWall {
                            last_state: self,
                            r: next_pos.0,
                            c: next_pos.1,
                            dir: direction,
                        });
                    }
                    Tile::Floor => {}
                }

                self.player = next_pos;
                self.moves += 1;
                return Ok(self);
            }
        }
        Err(SokobanError::InvalidMoveOOB {
            r: self.player.0,
            c: self.player.1,
            last_state: self,
            dir: direction,
        })
    }

    /// Iterator over the state in row-major order.
    #[must_use]
    pub fn iter(&self) -> StateIterator {
        StateIterator {
            state: self,
            inner: self.container.iter().enumerate(),
        }
    }

    /// The number of moves since the starting state of this state.
    #[must_use]
    pub fn moves(&self) -> usize {
        self.moves
    }

    /// The number of rows in this puzzle's map.
    #[must_use]
    pub fn rows(&self) -> usize {
        self.dim_r
    }

    /// The number of columns in this puzzle's map.
    #[must_use]
    pub fn cols(&self) -> usize {
        self.dim_c
    }

    /// The position of the player in the current state.
    #[must_use]
    pub fn player(&self) -> (usize, usize) {
        self.player
    }

    /// The position of the targets in the current state.
    #[must_use]
    pub fn targets(&self) -> &[(usize, usize)] {
        &self.targets
    }

    /// Parse a puzzle from the given reader. See [`Tile`] for details on the characters which
    /// represent puzzle tiles.
    ///
    /// # Errors
    ///
    /// Parsing fails under the following conditions:
    ///  - The number of columns in the puzzle are inconsistent
    ///  - A character not recognised as a tile appears
    ///  - Multiple players are on the map
    ///  - No players are on the map
    ///  - An IO error occurs while reading the puzzle
    ///
    pub fn parse<R>(reader: R) -> SokobanResult<Self>
    where
        R: BufRead,
    {
        let mut col = 0;
        let mut row = 0;
        let mut map = Vec::new();
        let mut player = None;
        let mut targets = Vec::new();
        for (line_no, line) in reader.lines().enumerate() {
            let line = line?;
            if line.is_empty() {
                continue; // lines may be blank
            }
            if col == 0 {
                col = line.len();
            } else if col != line.len() {
                return Err(SokobanError::InconsistentDimensions {
                    line: line_no,
                    expected: col,
                    found: line.len(),
                });
            }

            map.reserve(col);
            for (i, c) in line.char_indices() {
                match Tile::try_from(c) {
                    Ok(b) => map.push(b),
                    Err('M') => {
                        map.push(Tile::Crate);
                        targets.push((row, i));
                    }
                    Err('.') => {
                        map.push(Tile::Floor);
                        targets.push((row, i));
                    }
                    Err('X' | 'x') => {
                        map.push(Tile::Floor);
                        if let Some(player) = player {
                            return Err(SokobanError::MultiplePlayers {
                                first: player,
                                second: (row, i),
                            });
                        }
                        player = Some((row, i));
                        if c == 'X' {
                            targets.push((row, i));
                        }
                    }
                    Err(c) => {
                        return Err(SokobanError::InvalidChar {
                            found: c,
                            r: row,
                            c: i,
                        })
                    }
                }
            }
            row += 1;
        }

        if let Some(player) = player {
            Self::new(map, player, targets, row, col)
        } else {
            Err(SokobanError::MissingPlayer)
        }
    }
}

impl Index<(usize, usize)> for State {
    type Output = Tile;

    fn index(&self, index: (usize, usize)) -> &Self::Output {
        if let Some(index) = self.checked_bounds(index) {
            &self.container[index]
        } else {
            panic!("Index out of bounds: {index:?}")
        }
    }
}

impl Debug for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_char('\n')?;
        for (r, chunk) in self.container.chunks(self.dim_c).enumerate() {
            for (c, tile) in chunk.iter().enumerate() {
                if self.player == (r, c) {
                    if self.targets.contains(&(r, c)) {
                        f.write_char('X')?;
                    } else {
                        f.write_char('x')?;
                    }
                } else if self.targets.contains(&(r, c)) {
                    if tile == &Tile::Crate {
                        f.write_char('M')?;
                    } else {
                        f.write_char('.')?;
                    }
                } else {
                    tile.fmt(f)?;
                }
            }
            f.write_char('\n')?;
        }
        Ok(())
    }
}

impl Hash for State {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.targets.hash(state);
        self.dim_r.hash(state);
        self.dim_c.hash(state);
        self.container.hash(state);
        self.player.hash(state);
    }
}

/// Iterator over the map of the puzzle.
pub struct StateIterator<'a> {
    state: &'a State,
    inner: Enumerate<core::slice::Iter<'a, Tile>>,
}

impl<'a> Iterator for StateIterator<'a> {
    type Item = StateIteratorItem<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(index, &tile)| StateIteratorItem {
            state: self.state,
            index,
            tile,
        })
    }
}

/// An individual position in the state, as returned by a [`StateIterator`].
pub struct StateIteratorItem<'a> {
    state: &'a State,
    index: usize,
    tile: Tile,
}

impl<'a> StateIteratorItem<'a> {
    /// The tile at this position.
    #[must_use]
    pub fn tile(&self) -> Tile {
        self.tile
    }

    /// The position of this tile.
    #[must_use]
    pub fn position(&self) -> (usize, usize) {
        (self.index / self.state.dim_c, self.index % self.state.dim_c)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ops::IndexMut;

    impl IndexMut<(usize, usize)> for State {
        fn index_mut(&mut self, index: (usize, usize)) -> &mut Self::Output {
            if let Some(index) = self.checked_bounds(index) {
                &mut self.container[index]
            } else {
                panic!("Index out of bounds: {index:?}")
            }
        }
    }

    fn generate_basic() -> State {
        let rows = 5;
        let cols = 5;
        let mut raw = vec![Tile::Wall; rows * cols];
        let player = (2, 2);
        raw[11] = Tile::Floor;
        raw[12] = Tile::Floor;
        raw[13] = Tile::Floor;

        State::new(raw, player, Vec::new(), rows, cols).unwrap()
    }

    fn generate_basic_with_targets() -> State {
        let rows = 5;
        let cols = 5;
        let mut raw = vec![Tile::Wall; rows * cols];
        let player = (2, 2);
        raw[11] = Tile::Floor;
        raw[12] = Tile::Floor;
        raw[13] = Tile::Floor;
        let targets = vec![player];

        State::new(raw, player, targets, rows, cols).unwrap()
    }

    fn generate_hallway(width: usize) -> SokobanResult<State> {
        let rows = 3;
        let cols = width;
        let mut raw = vec![Tile::Wall; rows * cols];
        let player = (1, width / 2 - ((width + 1) % 2));
        raw.iter_mut()
            .skip(width + 1)
            .take(width - 2)
            .for_each(|tile| *tile = Tile::Floor);

        State::new(raw, player, Vec::new(), rows, cols)
    }

    fn generate_hallway_with_box(width: usize) -> SokobanResult<State> {
        generate_hallway(width).map(|mut state| {
            state[(1, (width / 2 - ((width + 1) % 2) + 1))] = Tile::Crate;
            state
        })
    }

    fn generate_hallway_with_hole(width: usize) -> SokobanResult<State> {
        generate_hallway(width).map(|mut state| {
            state[(1, 4)] = Tile::Floor;
            state
        })
    }

    fn generate_hallway_with_box_and_hole(width: usize) -> SokobanResult<State> {
        generate_hallway(width).map(|mut state| {
            state[(1, (width / 2 - ((width + 1) % 2) + 1))] = Tile::Crate;
            state[(1, 4)] = Tile::Floor;
            state
        })
    }

    fn generate_room(width: usize, targets: Vec<(usize, usize)>) -> SokobanResult<State> {
        let rows = width;
        let cols = width;
        let mut raw = vec![Tile::Wall; rows * cols];
        let player = (width / 2 - ((width + 1) % 2), width / 2 - ((width + 1) % 2));
        for i in ((width + 1)..(raw.len() - width)).step_by(width) {
            raw.iter_mut()
                .skip(i)
                .take(width - 2)
                .for_each(|tile| *tile = Tile::Floor);
        }

        State::new(raw, player, targets, rows, cols)
    }

    fn generate_room_with_box(width: usize, targets: Vec<(usize, usize)>) -> SokobanResult<State> {
        generate_room(width, targets).map(|mut state| {
            state[(
                width / 2 - ((width + 1) % 2) - 1,
                width / 2 - ((width + 1) % 2) + 1,
            )] = Tile::Crate;
            state
        })
    }

    #[test]
    fn basic_works() {
        let state = generate_basic();
        let debug_string = format!("{state:?}");
        assert_eq!(
            debug_string,
            r#"
#####
#####
#_x_#
#####
#####
"#
        );
    }

    #[test]
    fn basic_on_target_works() {
        let state = generate_basic_with_targets();
        let debug_string = format!("{state:?}");
        assert_eq!(
            debug_string,
            r#"
#####
#####
#_X_#
#####
#####
"#
        );
    }

    #[test]
    fn basic_invalid_wall_doesnt_work() {
        let state = State::new(vec![Tile::Wall], (0, 0), Vec::new(), 1, 1);
        if let SokobanError::InvalidStartingPosition { found, r, c } = state.unwrap_err() {
            assert_eq!(found, Some(Tile::Wall));
            assert_eq!(r, 0);
            assert_eq!(c, 0);
        } else {
            panic!("Unexpected failure condition; invalid error.")
        }
    }

    #[test]
    fn basic_invalid_crate_doesnt_work() {
        let state = State::new(vec![Tile::Crate], (0, 0), Vec::new(), 1, 1);
        if let SokobanError::InvalidStartingPosition { found, r, c } = state.unwrap_err() {
            assert_eq!(found, Some(Tile::Crate));
            assert_eq!(r, 0);
            assert_eq!(c, 0);
        } else {
            panic!("Unexpected failure condition; invalid error.")
        }
    }

    #[test]
    fn basic_invalid_oob_doesnt_work() {
        let state = State::new(vec![Tile::Wall], (0, 1), Vec::new(), 1, 1);
        if let SokobanError::InvalidStartingPosition { found, r, c } = state.unwrap_err() {
            assert_eq!(found, None);
            assert_eq!(r, 0);
            assert_eq!(c, 1);
        } else {
            panic!("Unexpected failure condition; invalid error.")
        }
    }

    #[test]
    fn invalid_target_wall_doesnt_work() {
        let raw = vec![
            Tile::Wall,
            Tile::Wall,
            Tile::Wall,
            Tile::Wall,
            Tile::Floor,
            Tile::Wall,
            Tile::Wall,
            Tile::Wall,
            Tile::Wall,
        ];
        let state = State::new(raw, (1, 1), vec![(0, 0)], 3, 3);
        if let SokobanError::InvalidTargetPosition { found, r, c } = state.unwrap_err() {
            assert_eq!(found, Some(Tile::Wall));
            assert_eq!(r, 0);
            assert_eq!(c, 0);
        } else {
            panic!("Unexpected failure condition; invalid error.")
        }
    }

    #[test]
    fn valid_target_crate_does_work() {
        let raw = vec![
            Tile::Crate,
            Tile::Wall,
            Tile::Wall,
            Tile::Wall,
            Tile::Floor,
            Tile::Wall,
            Tile::Wall,
            Tile::Wall,
            Tile::Wall,
        ];
        let state = State::new(raw, (1, 1), vec![(0, 0)], 3, 3);
        assert!(state.is_ok());
    }

    #[test]
    fn basic_invalid_target_oob_doesnt_work() {
        let state = State::new(vec![Tile::Floor], (0, 0), vec![(0, 1)], 1, 1);
        if let SokobanError::InvalidTargetPosition { found, r, c } = state.unwrap_err() {
            assert_eq!(found, None);
            assert_eq!(r, 0);
            assert_eq!(c, 1);
        } else {
            panic!("Unexpected failure condition; invalid error.")
        }
    }

    #[test]
    fn basic_solution_state_works() {
        let raw = vec![
            Tile::Crate,
            Tile::Wall,
            Tile::Wall,
            Tile::Wall,
            Tile::Floor,
            Tile::Wall,
            Tile::Wall,
            Tile::Wall,
            Tile::Wall,
        ];
        let state = State::new(raw, (1, 1), vec![(0, 0)], 3, 3);
        assert!(state.is_ok());
        assert!(state.unwrap().in_solution_state());
    }

    #[test]
    fn basic_not_solution_state_works() {
        let raw = vec![
            Tile::Crate,
            Tile::Wall,
            Tile::Wall,
            Tile::Wall,
            Tile::Floor,
            Tile::Wall,
            Tile::Wall,
            Tile::Wall,
            Tile::Wall,
        ];
        let state = State::new(raw, (1, 1), vec![(1, 1)], 3, 3);
        assert!(state.is_ok());
        assert!(!state.unwrap().in_solution_state());
    }

    #[test]
    fn move_left() {
        let last = std::iter::repeat(())
            .take(5)
            .fold(generate_hallway(20), |state, _| {
                state.and_then(|state| state.move_player(Direction::Left))
            });

        assert_eq!(
            format!("{:?}", last.unwrap()),
            r#"
####################
#___x______________#
####################
"#
        );
    }

    #[test]
    fn move_right_with_box() {
        let last = std::iter::repeat(())
            .take(5)
            .fold(generate_hallway_with_box(20), |state, _| {
                state.and_then(|state| state.move_player(Direction::Right))
            });
        let last = std::iter::repeat(()).take(5).fold(last, |state, _| {
            state.and_then(|state| state.move_player(Direction::Left))
        });

        assert_eq!(
            format!("{:?}", last.unwrap()),
            r#"
####################
#________x_____m___#
####################
"#
        );
    }

    #[test]
    fn move_up_right_with_box() {
        let last = std::iter::repeat(()).take(5).fold(
            generate_room_with_box(20, Vec::new()),
            |state, _| {
                state.and_then(|state| {
                    state
                        .move_player(Direction::Right)
                        .and_then(|state| state.move_player(Direction::Up))
                        .and_then(|state| state.move_player(Direction::Left))
                        .and_then(|state| state.move_player(Direction::Up))
                        .and_then(|state| state.move_player(Direction::Right))
                        .and_then(|state| state.move_player(Direction::Down))
                })
            },
        );

        assert_eq!(
            format!("{:?}", last.unwrap()),
            r#"
####################
#__________________#
#__________________#
#______________m___#
#_____________x____#
#__________________#
#__________________#
#__________________#
#__________________#
#__________________#
#__________________#
#__________________#
#__________________#
#__________________#
#__________________#
#__________________#
#__________________#
#__________________#
#__________________#
####################
"#
        );
    }

    #[test]
    fn hits_wall_and_dies() {
        let last = std::iter::repeat(())
            .take(5)
            .fold(generate_hallway(5), |state, _| {
                state.and_then(|state| state.move_player(Direction::Right))
            });
        if let SokobanError::InvalidMoveWall {
            last_state,
            r,
            c,
            dir,
        } = last.unwrap_err()
        {
            assert_eq!(last_state.moves(), 1);
            assert_eq!(r, 1);
            assert_eq!(c, 4);
            assert_eq!(dir, Direction::Right);
        } else {
            panic!("Unexpected failure condition; invalid error.")
        }
    }

    #[test]
    fn hits_crate_and_dies() {
        let last = std::iter::repeat(())
            .take(5)
            .fold(generate_hallway_with_box(5), |state, _| {
                state.and_then(|state| state.move_player(Direction::Right))
            });
        if let SokobanError::InvalidMoveCrate {
            last_state,
            r,
            c,
            dir,
        } = last.unwrap_err()
        {
            assert_eq!(last_state.moves(), 0);
            assert_eq!(r, 1);
            assert_eq!(c, 3);
            assert_eq!(dir, Direction::Right);
        } else {
            panic!("Unexpected failure condition; invalid error.")
        }
    }

    #[test]
    fn hits_hole_and_dies() {
        let last = std::iter::repeat(())
            .take(5)
            .fold(generate_hallway_with_hole(5), |state, _| {
                state.and_then(|state| state.move_player(Direction::Right))
            });
        if let SokobanError::InvalidMoveOOB {
            last_state,
            r,
            c,
            dir,
        } = last.unwrap_err()
        {
            assert_eq!(last_state.moves(), 2);
            assert_eq!(r, 1);
            assert_eq!(c, 4);
            assert_eq!(dir, Direction::Right);
        } else {
            panic!("Unexpected failure condition; invalid error.")
        }
    }

    #[test]
    fn pushes_crate_in_hole_and_dies() {
        let last = std::iter::repeat(())
            .take(5)
            .fold(generate_hallway_with_box_and_hole(5), |state, _| {
                state.and_then(|state| state.move_player(Direction::Right))
            });
        if let SokobanError::InvalidMoveCrate {
            last_state,
            r,
            c,
            dir,
        } = last.unwrap_err()
        {
            assert_eq!(last_state.moves(), 1);
            assert_eq!(r, 1);
            assert_eq!(c, 4);
            assert_eq!(dir, Direction::Right);
        } else {
            panic!("Unexpected failure condition; invalid error.")
        }
    }

    #[test]
    fn pushes_crate_to_target() {
        let result = generate_room_with_box(10, vec![(4, 5)])
            .and_then(|state| state.move_player(Direction::Up))
            .and_then(|state| state.move_player(Direction::Up))
            .and_then(|state| state.move_player(Direction::Right))
            .and_then(|state| state.move_player(Direction::Down))
            .map(|state| state.in_solution_state());

        if let Ok(solved) = result {
            assert!(solved);
        } else {
            panic!("Entered an invalid state.");
        }
    }
}
