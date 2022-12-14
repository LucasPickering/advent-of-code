use anyhow::anyhow;
use derive_more::{Add, AddAssign, Display, Sub, SubAssign};
use regex::{Captures, Match};
use std::{
    cmp,
    fmt::Debug,
    ops::{Add, AddAssign},
    str::FromStr,
};

/// Split a string on spaces
pub fn words(input: &str) -> Vec<&str> {
    input.split(' ').collect::<Vec<_>>()
}

/// Get the `(min, max)` of a pair
pub fn min_max<T: Ord>(a: T, b: T) -> (T, T) {
    if a < b {
        (a, b)
    } else {
        (b, a)
    }
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

impl FromStr for Position {
    type Err = anyhow::Error;

    /// Parse a string like x,y
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once(',') {
            Some((x, y)) => Ok(Self {
                x: x.parse()?,
                y: y.parse()?,
            }),
            None => Err(anyhow!("Invalid position string: {s}")),
        }
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

/// Extension trait for [std::iter::Iterator], supplying functionality related
/// to positions.
pub trait PositionIterator {
    /// Get the minimum and maximum x and y values that appear in a series of
    /// positions. The bounds are in the order of `(min_x, min_y, max_x,
    /// max_y)`. Returns `None` iff the iterator is empty.
    fn bounds(self) -> Option<(isize, isize, isize, isize)>;
}

impl<I: Iterator<Item = Position>> PositionIterator for I {
    fn bounds(self) -> Option<(isize, isize, isize, isize)> {
        self.fold(None, |acc, position| match acc {
            Some((min_x, min_y, max_x, max_y)) => Some((
                cmp::min(min_x, position.x),
                cmp::min(min_y, position.y),
                cmp::max(max_x, position.x),
                cmp::max(max_y, position.y),
            )),
            None => Some((position.x, position.y, position.x, position.y)),
        })
    }
}

/// Extension trait for [regex::Captures]
pub trait CapturesExt {
    /// Get a numbered capture group and unwrap the output
    fn get_unwrap(&self, i: usize) -> Match<'_>;

    /// Get a named capture group and unwrap the output
    fn name_unwrap(&self, name: &str) -> Match<'_>;
}

impl CapturesExt for Captures<'_> {
    fn get_unwrap(&self, i: usize) -> Match<'_> {
        self.get(i).unwrap()
    }

    fn name_unwrap(&self, name: &str) -> Match<'_> {
        self.name(name).unwrap()
    }
}

/// Extension trait for [regex::Match]
pub trait MatchExt {
    /// Parse the matched value into some parseable type, then unwrap that
    /// output
    fn parse_unwrap<T>(&self) -> T
    where
        T: FromStr,
        <T as FromStr>::Err: Debug;
}

impl MatchExt for Match<'_> {
    fn parse_unwrap<T>(&self) -> T
    where
        T: FromStr,
        <T as FromStr>::Err: Debug,
    {
        self.as_str().parse().unwrap()
    }
}
