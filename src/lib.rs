pub mod error;

use std::fmt::{Debug, Formatter, Write};
use std::ops::Index;
use crate::error::{SokobanError, SokobanResult};

#[derive(Copy, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub enum Block {
    Crate,
    Floor,
    Wall,
}

impl Debug for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Block::Crate => f.write_char('m'),
            Block::Floor => f.write_char('_'),
            Block::Wall => f.write_char('#')
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    pub fn go(&self, orig: (usize, usize)) -> (usize, usize) {
        match self {
            Direction::Up => (orig.0.wrapping_sub(1), orig.1),
            Direction::Down => (orig.0 + 1, orig.1),
            Direction::Left => (orig.0, orig.1.wrapping_sub(1)),
            Direction::Right => (orig.0, orig.1 + 1)
        }
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

#[derive(Clone)]
pub struct State {
    container: Vec<Block>,
    player: (usize, usize),
    targets: Vec<(usize, usize)>,
    moves: usize,
    dim_r: usize,
    dim_c: usize,
}

impl State {
    #[inline(always)]
    fn checked_bounds(&self, pos: (usize, usize)) -> Option<usize> {
        if pos.0 < self.dim_r && pos.1 < self.dim_c {
            self.dim_c.checked_mul(pos.0).and_then(|r| r.checked_add(pos.1))
        } else {
            None
        }
    }

    pub fn new(raw: Vec<Block>, player: (usize, usize), targets: Vec<(usize, usize)>, dim_r: usize, dim_c: usize) -> SokobanResult<Self> {
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
        let state = State { container: raw, player, targets, moves: 0, dim_r, dim_c };
        let checked_player = state.checked_bounds(state.player);
        if checked_player.is_none() || state.container[checked_player.unwrap()] != Block::Floor {
            return Err(SokobanError::InvalidStartingPosition {
                found: checked_player.map(|pos| state.container[pos]),
                r: player.0,
                c: player.1,
            });
        }
        for target in state.targets.iter().copied() {
            let checked_target = state.checked_bounds(target);
            if checked_target.is_none() || state.container[checked_target.unwrap()] == Block::Wall {
                return Err(SokobanError::InvalidTargetPosition {
                    found: checked_target.map(|pos| state.container[pos]),
                    r: target.0,
                    c: target.1,
                });
            }
        }
        Ok(state)
    }

    pub fn in_solution_state(&self) -> bool {
        self.targets.iter().copied().all(|target| self[target] == Block::Crate)
    }

    pub fn move_player(mut self, direction: Direction) -> SokobanResult<Self> {
        let next_pos = direction.go(self.player);
        if let Some(npi) = self.checked_bounds(next_pos) {
            match self.container[npi] {
                Block::Crate => {
                    let c_next_pos = direction.go(next_pos);
                    let cnpi = self.checked_bounds(c_next_pos);
                    if cnpi.is_none() || self.container[cnpi.unwrap()] != Block::Floor {
                        return Err(SokobanError::InvalidMoveCrate {
                            last_state: self,
                            r: next_pos.0,
                            c: next_pos.1,
                        });
                    }
                    let cnpi = cnpi.unwrap();
                    self.container[npi] = Block::Floor;
                    self.container[cnpi] = Block::Crate;
                }
                Block::Wall => {
                    return Err(SokobanError::InvalidMoveWall {
                        last_state: self,
                        r: next_pos.0,
                        c: next_pos.1,
                    });
                }
                Block::Floor => {}
            }

            self.player = next_pos;
            self.moves += 1;
            Ok(self)
        } else {
            Err(SokobanError::InvalidMoveOOB {
                r: self.player.0,
                c: self.player.1,
                last_state: self,
                dir: direction,
            })
        }
    }

    pub fn moves(&self) -> usize {
        self.moves
    }

    pub fn rows(&self) -> usize {
        self.dim_r
    }

    pub fn cols(&self) -> usize {
        self.dim_c
    }
}

impl Index<(usize, usize)> for State {
    type Output = Block;

    #[inline(always)]
    fn index(&self, index: (usize, usize)) -> &Self::Output {
        if let Some(index) = self.checked_bounds(index) {
            &self.container[index]
        } else {
            panic!("Index out of bounds: {:?}", index)
        }
    }
}

impl Debug for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_char('\n')?;
        for (r, chunk) in self.container.chunks(self.dim_c).enumerate() {
            for (c, block) in chunk.iter().enumerate() {
                if self.player == (r, c) {
                    if self.targets.contains(&(r, c)) {
                        f.write_char('X')?;
                    } else {
                        f.write_char('x')?;
                    }
                } else if self.targets.contains(&(r, c)) {
                    if block == &Block::Crate {
                        f.write_char('M')?;
                    } else {
                        f.write_char('.')?;
                    }
                } else {
                    block.fmt(f)?;
                }
            }
            f.write_char('\n')?
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::ops::IndexMut;
    use super::*;

    impl IndexMut<(usize, usize)> for State {
        #[inline(always)]
        fn index_mut(&mut self, index: (usize, usize)) -> &mut Self::Output {
            if let Some(index) = self.checked_bounds(index) {
                &mut self.container[index]
            } else {
                panic!("Index out of bounds: {:?}", index)
            }
        }
    }

    fn generate_basic() -> State {
        let rows = 5;
        let cols = 5;
        let mut raw = vec![Block::Wall; rows * cols];
        let player = (2, 2);
        raw[11] = Block::Floor;
        raw[12] = Block::Floor;
        raw[13] = Block::Floor;

        State::new(raw, player, Vec::new(), rows, cols).unwrap()
    }

    fn generate_basic_with_targets() -> State {
        let rows = 5;
        let cols = 5;
        let mut raw = vec![Block::Wall; rows * cols];
        let player = (2, 2);
        raw[11] = Block::Floor;
        raw[12] = Block::Floor;
        raw[13] = Block::Floor;
        let targets = vec![player];

        State::new(raw, player, targets, rows, cols).unwrap()
    }

    fn generate_hallway(width: usize) -> SokobanResult<State> {
        let rows = 3;
        let cols = width;
        let mut raw = vec![Block::Wall; rows * cols];
        let player = (1, width / 2 - ((width + 1) % 2));
        raw.iter_mut().skip(width + 1).take(width - 2).for_each(|block| *block = Block::Floor);

        State::new(raw, player, Vec::new(), rows, cols)
    }

    fn generate_hallway_with_box(width: usize) -> SokobanResult<State> {
        generate_hallway(width).map(|mut state| {
            state[(1, (width / 2 - ((width + 1) % 2) + 1))] = Block::Crate;
            state
        })
    }

    fn generate_hallway_with_hole(width: usize) -> SokobanResult<State> {
        generate_hallway(width).map(|mut state| {
            state[(1, 4)] = Block::Floor;
            state
        })
    }

    fn generate_hallway_with_box_and_hole(width: usize) -> SokobanResult<State> {
        generate_hallway(width).map(|mut state| {
            state[(1, (width / 2 - ((width + 1) % 2) + 1))] = Block::Crate;
            state[(1, 4)] = Block::Floor;
            state
        })
    }

    fn generate_room(width: usize, targets: Vec<(usize, usize)>) -> SokobanResult<State> {
        let rows = width;
        let cols = width;
        let mut raw = vec![Block::Wall; rows * cols];
        let player = (width / 2 - ((width + 1) % 2), width / 2 - ((width + 1) % 2));
        for i in ((width + 1)..(raw.len() - width)).step_by(width) {
            raw.iter_mut().skip(i).take(width - 2).for_each(|block| *block = Block::Floor);
        }

        State::new(raw, player, targets, rows, cols)
    }

    fn generate_room_with_box(width: usize, targets: Vec<(usize, usize)>) -> SokobanResult<State> {
        generate_room(width, targets).map(|mut state| {
            state[(width / 2 - ((width + 1) % 2) - 1, width / 2 - ((width + 1) % 2) + 1)] = Block::Crate;
            state
        })
    }

    #[test]
    fn basic_works() {
        let state = generate_basic();
        let debug_string = format!("{:?}", state);
        assert_eq!(debug_string, r#"
#####
#####
#_x_#
#####
#####
"#);
    }

    #[test]
    fn basic_on_target_works() {
        let state = generate_basic_with_targets();
        let debug_string = format!("{:?}", state);
        assert_eq!(debug_string, r#"
#####
#####
#_X_#
#####
#####
"#);
    }

    #[test]
    fn basic_invalid_wall_doesnt_work() {
        let state = State::new(vec![Block::Wall], (0, 0), Vec::new(), 1, 1);
        if let SokobanError::InvalidStartingPosition { found, r, c } = state.unwrap_err() {
            assert_eq!(found, Some(Block::Wall));
            assert_eq!(r, 0);
            assert_eq!(c, 0);
        } else {
            panic!("Unexpected failure condition; invalid error.")
        }
    }

    #[test]
    fn basic_invalid_crate_doesnt_work() {
        let state = State::new(vec![Block::Crate], (0, 0), Vec::new(), 1, 1);
        if let SokobanError::InvalidStartingPosition { found, r, c } = state.unwrap_err() {
            assert_eq!(found, Some(Block::Crate));
            assert_eq!(r, 0);
            assert_eq!(c, 0);
        } else {
            panic!("Unexpected failure condition; invalid error.")
        }
    }

    #[test]
    fn basic_invalid_oob_doesnt_work() {
        let state = State::new(vec![Block::Wall], (0, 1), Vec::new(), 1, 1);
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
            Block::Wall, Block::Wall, Block::Wall,
            Block::Wall, Block::Floor, Block::Wall,
            Block::Wall, Block::Wall, Block::Wall,
        ];
        let state = State::new(raw, (1, 1), vec![(0, 0)], 3, 3);
        if let SokobanError::InvalidTargetPosition { found, r, c } = state.unwrap_err() {
            assert_eq!(found, Some(Block::Wall));
            assert_eq!(r, 0);
            assert_eq!(c, 0);
        } else {
            panic!("Unexpected failure condition; invalid error.")
        }
    }

    #[test]
    fn valid_target_crate_does_work() {
        let raw = vec![
            Block::Crate, Block::Wall, Block::Wall,
            Block::Wall, Block::Floor, Block::Wall,
            Block::Wall, Block::Wall, Block::Wall,
        ];
        let state = State::new(raw, (1, 1), vec![(0, 0)], 3, 3);
        assert!(state.is_ok());
    }

    #[test]
    fn basic_invalid_target_oob_doesnt_work() {
        let state = State::new(vec![Block::Floor], (0, 0), vec![(0, 1)], 1, 1);
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
            Block::Crate, Block::Wall, Block::Wall,
            Block::Wall, Block::Floor, Block::Wall,
            Block::Wall, Block::Wall, Block::Wall,
        ];
        let state = State::new(raw, (1, 1), vec![(0, 0)], 3, 3);
        assert!(state.is_ok());
        assert!(state.unwrap().in_solution_state());
    }

    #[test]
    fn basic_not_solution_state_works() {
        let raw = vec![
            Block::Crate, Block::Wall, Block::Wall,
            Block::Wall, Block::Floor, Block::Wall,
            Block::Wall, Block::Wall, Block::Wall,
        ];
        let state = State::new(raw, (1, 1), vec![(1, 1)], 3, 3);
        assert!(state.is_ok());
        assert!(!state.unwrap().in_solution_state());
    }

    #[test]
    fn move_left() {
        let last = std::iter::repeat(()).take(5).fold(generate_hallway(20), |state, _| state.and_then(|state| {
            state.move_player(Direction::Left)
        }));

        assert_eq!(format!("{:?}", last.unwrap()), r#"
####################
#___x______________#
####################
"#);
    }

    #[test]
    fn move_right_with_box() {
        let last = std::iter::repeat(()).take(5).fold(generate_hallway_with_box(20), |state, _| state.and_then(|state| {
            state.move_player(Direction::Right)
        }));
        let last = std::iter::repeat(()).take(5).fold(last, |state, _| state.and_then(|state| {
            state.move_player(Direction::Left)
        }));

        assert_eq!(format!("{:?}", last.unwrap()), r#"
####################
#________x_____m___#
####################
"#);
    }

    #[test]
    fn move_up_right_with_box() {
        let last = std::iter::repeat(()).take(5).fold(generate_room_with_box(20, Vec::new()), |state, _| state.and_then(|state| {
            state.move_player(Direction::Right)
                .and_then(|state| {
                    state.move_player(Direction::Up)
                })
                .and_then(|state| {
                    state.move_player(Direction::Left)
                })
                .and_then(|state| {
                    state.move_player(Direction::Up)
                })
                .and_then(|state| {
                    state.move_player(Direction::Right)
                })
                .and_then(|state| {
                    state.move_player(Direction::Down)
                })
        }));

        assert_eq!(format!("{:?}", last.unwrap()), r#"
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
"#);
    }

    #[test]
    fn hits_wall_and_dies() {
        let last = std::iter::repeat(()).take(5).fold(generate_hallway(5), |state, _| state.and_then(|state| {
            state.move_player(Direction::Right)
        }));
        if let SokobanError::InvalidMoveWall { last_state, r, c } = last.unwrap_err() {
            assert_eq!(last_state.moves(), 1);
            assert_eq!(r, 1);
            assert_eq!(c, 4);
        } else {
            panic!("Unexpected failure condition; invalid error.")
        }
    }

    #[test]
    fn hits_crate_and_dies() {
        let last = std::iter::repeat(()).take(5).fold(generate_hallway_with_box(5), |state, _| state.and_then(|state| {
            state.move_player(Direction::Right)
        }));
        if let SokobanError::InvalidMoveCrate { last_state, r, c } = last.unwrap_err() {
            assert_eq!(last_state.moves(), 0);
            assert_eq!(r, 1);
            assert_eq!(c, 3);
        } else {
            panic!("Unexpected failure condition; invalid error.")
        }
    }

    #[test]
    fn hits_hole_and_dies() {
        let last = std::iter::repeat(()).take(5).fold(generate_hallway_with_hole(5), |state, _| state.and_then(|state| {
            state.move_player(Direction::Right)
        }));
        if let SokobanError::InvalidMoveOOB { last_state, dir, r, c } = last.unwrap_err() {
            assert_eq!(last_state.moves(), 2);
            assert_eq!(dir, Direction::Right);
            assert_eq!(r, 1);
            assert_eq!(c, 4);
        } else {
            panic!("Unexpected failure condition; invalid error.")
        }
    }

    #[test]
    fn pushes_crate_in_hole_and_dies() {
        let last = std::iter::repeat(()).take(5).fold(generate_hallway_with_box_and_hole(5), |state, _| state.and_then(|state| {
            state.move_player(Direction::Right)
        }));
        if let SokobanError::InvalidMoveCrate { last_state, r, c } = last.unwrap_err() {
            assert_eq!(last_state.moves(), 1);
            assert_eq!(r, 1);
            assert_eq!(c, 4);
        } else {
            panic!("Unexpected failure condition; invalid error.")
        }
    }

    #[test]
    fn pushes_crate_to_target() {
        let result = generate_room_with_box(10, vec![(4, 5)]).and_then(|state| {
            state.move_player(Direction::Up)
        }).and_then(|state| {
            state.move_player(Direction::Up)
        }).and_then(|state| {
            state.move_player(Direction::Right)
        }).and_then(|state| {
            state.move_player(Direction::Down)
        }).map(|state| {
            state.in_solution_state()
        });

        if let Ok(solved) = result {
            assert!(solved);
        } else {
            panic!("Entered an invalid state.");
        }
    }
}
