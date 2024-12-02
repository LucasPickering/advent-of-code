use std::collections::HashMap;

pub struct Solver;

impl super::Solver for Solver {
    fn part1(&self, input: String) -> String {
        let (mut l1, mut l2) = parse_input(&input);
        l1.sort();
        l2.sort();
        let answer: u32 =
            l1.into_iter().zip(l2).map(|(a, b)| a.abs_diff(b)).sum();
        answer.to_string()
    }

    fn part2(&self, input: String) -> String {
        let (l1, l2) = parse_input(&input);
        let counts = count(l2);
        let score: u32 = l1
            .into_iter()
            .map(|value| counts.get(&value).unwrap_or(&0) * value)
            .sum();
        score.to_string()
    }
}

fn parse_input(input: &str) -> (Vec<u32>, Vec<u32>) {
    input
        .lines()
        .map(|line| {
            let (a, b) = line.split_once("   ").expect("Invalid line");
            let a: u32 = a.parse().unwrap();
            let b: u32 = b.parse().unwrap();
            (a, b)
        })
        .unzip()
}

/// Counts instances of each value in the iterator
fn count(l: impl IntoIterator<Item = u32>) -> HashMap<u32, u32> {
    l.into_iter().fold(HashMap::new(), |mut acc, value| {
        acc.entry(value)
            .and_modify(|count| *count += 1)
            .or_insert(1);
        acc
    })
}
