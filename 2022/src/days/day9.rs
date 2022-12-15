use crate::util::{words, Direction, Position, PositionIterator};
use log::{debug, trace};
use std::{collections::HashSet, fmt::Display, iter};

pub struct Solver;

impl super::Solver for Solver {
    fn part1(&self, input: String) -> String {
        let actions = parse_input(&input);
        let mut state = State::with_length(2);
        state.apply_actions(&actions);
        state.all_tails_unique().len().to_string()
    }

    fn part2(&self, input: String) -> String {
        let actions = parse_input(&input);
        let mut state = State::with_length(10);
        debug!("{}", state);
        state.apply_actions(&actions);
        state.all_tails_unique().len().to_string()
    }
}

#[derive(Copy, Clone, Debug)]
struct Action {
    direction: Direction,
    quantity: usize,
}

#[derive(Clone, Debug)]
struct Rope {
    knots: Vec<Position>,
}

#[derive(Clone, Debug)]
struct State {
    rope: Rope,
    history: Vec<Rope>,
}

fn parse_input(input: &str) -> Vec<Action> {
    input
        .lines()
        .map(|line| match words(line).as_slice() {
            &[direction, quantity] => Action {
                direction: direction.parse().unwrap(),
                quantity: quantity.parse().unwrap(),
            },
            _ => panic!("Unparseable line: {}", line),
        })
        .collect()
}

impl Rope {
    fn head(&self) -> Position {
        self.knots[0]
    }

    fn tail(&self) -> Position {
        self.knots[self.knots.len() - 1]
    }
}

impl State {
    /// Initialize new rope state, where the rope has the given number of knots
    fn with_length(len: usize) -> Self {
        Self {
            rope: Rope {
                knots: iter::repeat_with(Position::default).take(len).collect(),
            },
            history: Vec::new(),
        }
    }

    fn apply_actions(&mut self, actions: &[Action]) {
        for action in actions {
            for _ in 0..action.quantity {
                self.move_in_direction(action.direction);
                trace!("{}", self);
            }
            debug!("{}", self);
        }
    }

    /// Move the head of the rope one tile in a direction
    fn move_in_direction(&mut self, direction: Direction) {
        // Archive this, for posterity
        self.history.push(self.rope.clone());

        // Move the head in the given direction
        self.rope.knots[0] += direction;

        // Iterate through each tail knot, and see if it needs to be moved to
        // stay adjacent to its parent
        for i in 1..self.rope.knots.len() {
            // This is safe since we start at i=1
            let parent = self.rope.knots[i - 1];
            let knot = &mut self.rope.knots[i];

            // If the knot isn't touching its parent anymore, move it according
            // to Planck's Laws of Motion. The parent can only ever move one
            // cell at a time in each direction, so we don't need to worry about
            // distances more than 2
            let diff = parent - *knot;
            match diff.abs().into() {
                // Parent is in the same row/column - move one step toward them
                (2, 0) | (0, 2) => {
                    *knot += Position::from((diff.x.signum(), diff.y.signum()))
                }
                // Parent is diagonal, move diagonally
                (2, 1) | (1, 2) | (2, 2) => {
                    *knot += Position::from((
                        diff.x / diff.x.abs(),
                        diff.y / diff.y.abs(),
                    ))
                }
                // If this knot didn't move, then none of its children will
                // either, so we can stop looping
                (0, 0) | (1, 0) | (0, 1) | (1, 1) => break,
                other => panic!(
                    "Unexpected motion: {:?} [{} => {}]",
                    other, parent, knot
                ),
            }
        }
    }

    /// Get all ropes in the state history, including the current one
    fn all_ropes(&self) -> impl Iterator<Item = &Rope> + '_ {
        self.history.iter().chain(iter::once(&self.rope))
    }

    /// Get the entire history of tail positions for this state, unique-ified
    fn all_tails_unique(&self) -> HashSet<Position> {
        self.all_ropes()
            .map(|rope| rope.tail())
            .collect::<HashSet<_>>()
    }
}

impl Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Find the bounds of the positions visited in history, to figure out
        // how big our diagram is going to be
        let (min_x, min_y, max_x, max_y) = self
            .all_ropes()
            .flat_map(|rope| &rope.knots)
            .copied()
            .bounds()
            .unwrap();
        let tail_history = self
            .history
            .iter()
            .map(|rope| rope.tail())
            .collect::<HashSet<_>>();

        // Y goes bottom-to-top, so iterate in reverse
        for y in (min_y..=max_y).rev() {
            for x in min_x..=max_x {
                let position = (x, y).into();
                let c = if position == self.rope.head() {
                    'H'
                } else if let Some(index) =
                    self.rope.knots.iter().position(|pos| *pos == position)
                {
                    // Tail knots get numbered, with 1 next to the head
                    char::from_digit(index as u32, 10).unwrap()
                } else if position == (0, 0).into() {
                    's'
                } else if tail_history.contains(&position) {
                    '#'
                } else {
                    '.'
                };
                write!(f, "{}", c)?;
            }
            writeln!(f)?;
        }

        Ok(())
    }
}
