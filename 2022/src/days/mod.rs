pub mod day1;
pub mod day2;
pub mod day3;

/// A day's solvers. &self is needed so it can be a trait object
pub trait Solver {
    fn part1(&self, input: String) -> anyhow::Result<String>;
    fn part2(&self, input: String) -> anyhow::Result<String>;
}
