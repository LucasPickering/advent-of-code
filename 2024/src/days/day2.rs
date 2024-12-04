use crate::tui::Tui;
use itertools::Itertools;

pub struct Solver {
    reports: Vec<Report>,
}

type Report = Vec<i32>;

impl super::Solver for Solver {
    fn new(input: String, _: Tui) -> Self {
        let reports = input
            .lines()
            .map(|line| {
                line.split(" ")
                    .map(|value| value.parse::<i32>().expect("Invalid value"))
                    .collect()
            })
            .collect();
        Self { reports }
    }

    fn part1(self: Box<Self>) -> String {
        let num_safe = self
            .reports
            .iter()
            .filter(|report| is_safe_p1(report.iter().copied()))
            .count();
        num_safe.to_string()
    }

    fn part2(self: Box<Self>) -> String {
        let num_safe = self
            .reports
            .iter()
            .filter(|report| is_safe_p2(report))
            .count();
        num_safe.to_string()
    }
}

fn is_safe_p1(report: impl Iterator<Item = i32>) -> bool {
    let mut direction: Option<bool> = None;
    report.tuple_windows().all(|(a, b)| {
        let change = a < b;
        let direction = direction.get_or_insert(change);
        *direction == change && (1..=3).contains(&a.abs_diff(b))
    })
}

fn is_safe_p2(report: &Report) -> bool {
    for skip in 0..report.len() {
        let is_safe = is_safe_p1(
            report
                .iter()
                .enumerate()
                .filter(|(i, _)| *i != skip)
                .map(|(_, n)| *n),
        );
        if is_safe {
            return true;
        }
    }
    false
}
