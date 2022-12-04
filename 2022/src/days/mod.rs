mod day1;
mod day2;
mod day3;
mod day4;

use crate::days::{
    day1::Day1Solver, day2::Day2Solver, day3::Day3Solver, day4::Day4Solver,
};

/// A day's solvers. &self is needed so it can be a trait object
pub trait Solver {
    fn part1(&self, input: String) -> String;
    fn part2(&self, input: String) -> String;
}

pub fn get_solver(day: u8) -> Box<dyn Solver> {
    match day {
        1 => Box::new(Day1Solver),
        2 => Box::new(Day2Solver),
        3 => Box::new(Day3Solver),
        4 => Box::new(Day4Solver),
        // Add new days here
        _ => panic!("Invalid day: {day}"),
    }
}
