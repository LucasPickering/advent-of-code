use std::collections::HashMap;

pub struct Solver {
    list1: Vec<u32>,
    list2: Vec<u32>,
}

impl super::Solver for Solver {
    fn new(input: String) -> Self {
        let (list1, list2) = input
            .lines()
            .map(|line| {
                let (a, b) = line.split_once("   ").expect("Invalid line");
                let a: u32 = a.parse().unwrap();
                let b: u32 = b.parse().unwrap();
                (a, b)
            })
            .unzip();
        Self { list1, list2 }
    }

    fn part1(mut self: Box<Self>) -> String {
        self.list1.sort();
        self.list2.sort();
        let answer: u32 = self
            .list1
            .into_iter()
            .zip(self.list2)
            .map(|(a, b)| a.abs_diff(b))
            .sum();
        answer.to_string()
    }

    fn part2(self: Box<Self>) -> String {
        let counts = count(self.list2);
        let score: u32 = self
            .list1
            .into_iter()
            .map(|value| counts.get(&value).unwrap_or(&0) * value)
            .sum();
        score.to_string()
    }
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
