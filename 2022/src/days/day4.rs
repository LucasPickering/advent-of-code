use crate::days::Solver;
use anyhow::{anyhow, Context};
use std::ops::RangeInclusive;

pub struct Day4Solver;

impl Solver for Day4Solver {
    fn part1(&self, input: String) -> String {
        let range_pairs = parse_input(input);
        range_pairs
            .iter()
            .filter(|(first, second)| has_subset(first, second))
            .count()
            .to_string()
    }

    fn part2(&self, input: String) -> String {
        let range_pairs = parse_input(input);
        range_pairs
            .iter()
            .filter(|(first, second)| has_overlap(first, second))
            .count()
            .to_string()
    }
}

type SectionRange = RangeInclusive<u32>;

fn parse_input(input: String) -> Vec<(SectionRange, SectionRange)> {
    input
        .lines()
        .map(|line| {
            let (first, second) = line
                .split_once(',')
                .ok_or_else(|| anyhow!("Error parsing line: {line}"))
                .unwrap();
            (parse_range(first), parse_range(second))
        })
        .collect()
}

fn parse_range(range: &str) -> SectionRange {
    let (min, max) = range
        .split_once('-')
        .ok_or_else(|| anyhow!("Error parsing range: {range}"))
        .unwrap();
    let min: u32 = min.parse().context("Error parsing range min").unwrap();
    let max: u32 = max.parse().context("Error parsing range max").unwrap();
    min..=max
}

/// Check if one range is a total subset of the other. This will check in both
/// directions, and return true if either is a subset of the other
fn has_subset(first: &SectionRange, second: &SectionRange) -> bool {
    first.contains(second.start()) && first.contains(second.end())
        || second.contains(first.start()) && second.contains(first.end())
}

/// Check if two ranges intersect at all
fn has_overlap(first: &SectionRange, second: &SectionRange) -> bool {
    first.contains(second.start())
        || first.contains(second.end())
        || second.contains(first.start())
        || second.contains(first.end())
}
