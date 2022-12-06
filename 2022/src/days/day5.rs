use crate::days::Solver;
use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;
use std::{collections::HashMap, fmt::Display};

pub struct Day5Solver;

impl Solver for Day5Solver {
    fn part1(&self, input: String) -> String {
        let (mut stacks, moves) = parse_input(&input);
        for move_ in moves {
            apply_move_single(&mut stacks, move_);
        }
        stacks.top_crates()
    }

    fn part2(&self, input: String) -> String {
        let (mut stacks, moves) = parse_input(&input);
        for move_ in moves {
            apply_move_multi(&mut stacks, move_);
        }
        stacks.top_crates()
    }
}

type Crate = char;
type Stack = Vec<Crate>;

#[derive(Clone, Debug)]
struct Stacks(HashMap<usize, Stack>);

#[derive(Copy, Clone, Debug)]
struct Move {
    quantity: usize,
    from: usize,
    to: usize,
}

fn parse_input(input: &str) -> (Stacks, Vec<Move>) {
    lazy_static! {
        static ref CRATE_REGEX: Regex =
            Regex::new(r"(?:\[(\w)\])|(?:   ) ?").unwrap();
        static ref STACK_LABEL_REGEX: Regex =
            Regex::new(r" \d (  \d )*\n\n").unwrap();
        static ref MOVE_REGEX: Regex =
            Regex::new(r"move (\d+) from (\d+) to (\d+)").unwrap();
    }

    // Start by finding the list of crate labels, which splits stacks and moves
    let stack_labels_match = STACK_LABEL_REGEX.find(input).unwrap();

    // Everything about the stack labels is the stacks
    // Everything below is the moves
    let stack_lines = &input[..stack_labels_match.start()];
    let move_lines = &input[stack_labels_match.end()..];

    // Parse stacks. We reverse the lines so we can go bottom-to-top
    let mut stacks = HashMap::new();
    for stack_line in stack_lines.lines().rev() {
        for (i, caps) in CRATE_REGEX.captures_iter(stack_line).enumerate() {
            let stack_num = i + 1;
            // If this stack has a crate at this level, then it will be captured
            // as group #1
            if let Some(crate_label) = caps.get(1) {
                let crate_label = crate_label.as_str().chars().next().unwrap();
                stacks
                    .entry(stack_num)
                    .or_insert_with(Vec::new)
                    .push(crate_label);
            }
        }
    }

    // Parse moves
    let moves = move_lines
        .lines()
        .map(|line| {
            let captures = MOVE_REGEX.captures(line).unwrap();
            Move {
                quantity: captures[1].parse().unwrap(),
                from: captures[2].parse().unwrap(),
                to: captures[3].parse().unwrap(),
            }
        })
        .collect();

    (Stacks(stacks), moves)
}

/// Apply a move, where crates are moved one at a time, and therefore reversed
/// (part 1)
fn apply_move_single(stacks: &mut Stacks, move_: Move) {
    for _ in 0..move_.quantity {
        let crate_ = stacks.0.get_mut(&move_.from).unwrap().pop().unwrap();
        stacks.0.get_mut(&move_.to).unwrap().push(crate_);
    }
}

/// Apply a move, where crates are moved all at once, keeping their order (part
/// 2)
fn apply_move_multi(stacks: &mut Stacks, move_: Move) {
    let from_stack = stacks.0.get_mut(&move_.from).unwrap();
    let crates = from_stack.split_off(from_stack.len() - move_.quantity);
    let to_stack = stacks.0.get_mut(&move_.to).unwrap();
    to_stack.extend(crates);
}

impl Stacks {
    /// Get the crate on top of each stack, as a string
    fn top_crates(&self) -> String {
        self.0
            .iter()
            .sorted_by_key(|(num, _)| **num)
            .map(|(_, stack)| stack.last().copied().unwrap_or(' '))
            .collect()
    }
}

impl Display for Stacks {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let num_stacks = self.0.len();
        let highest_stack =
            self.0.values().map(Vec::len).max().unwrap_or_default();

        // Count down from the top of the highest stack to the bottom
        for crate_index in (0..highest_stack).rev() {
            // Move left-to-right on stacks
            for num_stack in 1..=num_stacks {
                let stack = self.0.get(&num_stack).unwrap();
                match stack.get(crate_index) {
                    Some(crate_) => write!(f, "[{}] ", crate_)?,
                    None => write!(f, "    ")?,
                }
            }
            writeln!(f)?;
        }

        // Write the crate labels at the bottom
        for num_stack in 1..=num_stacks {
            write!(f, " {num_stack}  ")?;
        }
        Ok(())
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -{}-> {}", self.from, self.quantity, self.to)
    }
}
