use crate::{
    tui::Tui,
    util::{count_digits, split_digits},
};
use std::collections::HashMap;

pub struct Solver {
    stones: Vec<Stone>,
}

type Stone = u64;

/// Cache how many stones a particular number will turn into after n iterations
type Cache = HashMap<CacheKey, usize>;

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
struct CacheKey {
    stone: Stone,
    iterations: usize,
}

/// Result of one iteration
enum StoneSplit {
    One(Stone),
    Two(Stone, Stone),
}

impl super::Solver for Solver {
    fn new(input: String, _: Tui) -> Self {
        let stones = input
            .trim()
            .split(' ')
            .map(|token| token.parse::<Stone>().expect("invalid number"))
            .collect();
        Self { stones }
    }

    fn part1(self: Box<Self>) -> String {
        self.count_stones(25).to_string()
    }

    fn part2(self: Box<Self>) -> String {
        self.count_stones(75).to_string()
    }
}

impl Solver {
    fn count_stones(&self, iterations: usize) -> usize {
        let mut cache: Cache = HashMap::with_capacity(1_000_000);
        self.stones
            .iter()
            .map(|stone| count_splits(*stone, iterations, &mut cache))
            .sum::<usize>()
    }
}

/// Count how many stones this has split into after some number of iterations
fn count_splits(stone: Stone, iterations: usize, cache: &mut Cache) -> usize {
    let cache_key = CacheKey { stone, iterations };
    if let Some(total) = cache.get(&cache_key) {
        *total
    } else if iterations == 0 {
        1
    } else {
        // Value is not cached yet, we need to calculate and cache it now
        let total = match update_stone(stone) {
            StoneSplit::One(stone) => {
                count_splits(stone, iterations - 1, cache)
            }
            StoneSplit::Two(a, b) => {
                count_splits(a, iterations - 1, cache)
                    + count_splits(b, iterations - 1, cache)
            }
        };
        cache.insert(cache_key, total);
        total
    }
}

fn update_stone(stone: Stone) -> StoneSplit {
    if stone == 0 {
        StoneSplit::One(1)
    } else if count_digits(stone) % 2 == 0 {
        let (a, b) = split_digits(stone, count_digits(stone) / 2);
        StoneSplit::Two(a, b)
    } else {
        StoneSplit::One(stone * 2024)
    }
}
