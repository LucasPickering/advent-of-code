use crate::days::Solver;
use derive_more::{Display, From, FromStr};
use itertools::Itertools;
use std::{collections::HashSet, fmt::Debug};

pub struct Day3Solver;

impl Solver for Day3Solver {
    fn part1(&self, input: String) -> String {
        let rucksacks = parse_input(input);
        let duplicates_sum: u32 = rucksacks
            .iter()
            .map(find_duplicate)
            .map(ItemType::priority)
            .map(u32::from)
            .sum();
        duplicates_sum.to_string()
    }

    fn part2(&self, input: String) -> String {
        let rucksacks = parse_input(input);
        let badges_sum: u32 = rucksacks
            .into_iter()
            .chunks(3)
            .into_iter()
            .map(|elf_group| {
                match elf_group
                    .map(|(first_compartment, second_compartment)| {
                        &first_compartment | &second_compartment
                    })
                    .collect::<Vec<HashSet<_>>>()
                    .as_slice()
                {
                    [first, second, third] => {
                        unwrap_set(&(first & second) & third)
                    }
                    other => panic!("Invalid elf group: {other:?}"),
                }
            })
            .map(ItemType::priority)
            .map(u32::from)
            .sum();
        badges_sum.to_string()
    }
}

#[derive(Copy, Clone, Debug, Display, Eq, From, FromStr, Hash, PartialEq)]
struct ItemType(char);

impl ItemType {
    fn priority(self) -> u8 {
        if self.0.is_ascii_lowercase() {
            // a-z is 0-26
            self.0 as u8 - b'a' + 1
        } else if self.0.is_ascii_uppercase() {
            // A-Z is 27-52
            self.0 as u8 - b'A' + 27
        } else {
            // Everything else is invalid
            panic!("Unknown item type: {}", self.0)
        }
    }
}

type Compartment = HashSet<ItemType>;
type Rucksack = (Compartment, Compartment);

fn parse_input(input: String) -> Vec<Rucksack> {
    input
        .lines()
        .map(|line| {
            let half = line.len() / 2;
            let first_compartment =
                line[..half].chars().map(ItemType).collect();
            let second_compartment =
                line[half..].chars().map(ItemType).collect();
            (first_compartment, second_compartment)
        })
        .collect()
}

fn find_duplicate(
    (first_compartment, second_compartment): &Rucksack,
) -> ItemType {
    // There should be exactly one shared item type between the two comparements
    unwrap_set(first_compartment & second_compartment)
}

/// Unwrap a single element from a set
fn unwrap_set<T: Copy + Debug>(set: HashSet<T>) -> T {
    if set.len() == 1 {
        *set.iter().next().unwrap()
    } else {
        panic!("Cannot unwrap set: {set:?}")
    }
}
