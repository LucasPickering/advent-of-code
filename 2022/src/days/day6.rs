use crate::days::Solver;
use std::collections::HashSet;

pub struct Day6Solver;

impl Solver for Day6Solver {
    fn part1(&self, input: String) -> String {
        find_distinct(&input, 4).to_string()
    }

    fn part2(&self, input: String) -> String {
        find_distinct(&input, 14).to_string()
    }
}

fn find_distinct(input: &str, window_size: usize) -> usize {
    for start in 0..(input.len() - window_size) {
        let end = start + window_size;
        let slice = &input[start..end];
        if HashSet::<char>::from_iter(slice.chars()).len() == window_size {
            return end;
        }
    }
    unreachable!();
}
