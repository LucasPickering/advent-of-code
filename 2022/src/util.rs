use anyhow::anyhow;
use derive_more::{Add, AddAssign, Display, Sub, SubAssign};
use std::{
    ops::{Add, AddAssign},
    str::FromStr,
};

/// Split a string on spaces
pub fn words(input: &str) -> Vec<&str> {
    input.split(' ').collect::<Vec<_>>()
}

/// 2D position
#[derive(
    Add,
    AddAssign,
    Copy,
    Clone,
    Debug,
    Default,
    Display,
    Eq,
    Hash,
    PartialEq,
    Sub,
    SubAssign,
)]
#[display(fmt = "({x}, {y})")]
pub struct Position {
    pub x: isize,
    pub y: isize,
}

impl Position {
    /// Absolute value of the position
    pub fn abs(self) -> Self {
        Self {
            x: self.x.abs(),
            y: self.y.abs(),
        }
    }
}

impl From<(isize, isize)> for Position {
    fn from((x, y): (isize, isize)) -> Self {
        Self { x, y }
    }
}

impl From<Position> for (isize, isize) {
    fn from(position: Position) -> Self {
        (position.x, position.y)
    }
}

impl From<Direction> for Position {
    fn from(direction: Direction) -> Self {
        match direction {
            Direction::Up => (0, 1).into(),
            Direction::Down => (0, -1).into(),
            Direction::Left => (-1, 0).into(),
            Direction::Right => (1, 0).into(),
        }
    }
}

impl Add<Direction> for Position {
    type Output = Position;

    fn add(self, direction: Direction) -> Self::Output {
        self + Position::from(direction)
    }
}

impl AddAssign<Direction> for Position {
    fn add_assign(&mut self, direction: Direction) {
        self.add_assign(Position::from(direction));
    }
}

/// 2D direction
#[derive(Copy, Clone, Debug)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    pub fn all() -> [Self; 4] {
        [
            Direction::Up,
            Direction::Down,
            Direction::Left,
            Direction::Right,
        ]
    }
}

impl FromStr for Direction {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "U" => Ok(Self::Up),
            "D" => Ok(Self::Down),
            "L" => Ok(Self::Left),
            "R" => Ok(Self::Right),
            other => Err(anyhow!("Unknown direction: {}", other)),
        }
    }
}
