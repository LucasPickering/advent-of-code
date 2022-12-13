mod day1;
mod day10;
mod day11;
mod day12;
mod day13;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;

use crate::days::{
    day1::Day1Solver, day10::Day10Solver, day11::Day11Solver,
    day12::Day12Solver, day13::Day13Solver, day2::Day2Solver, day3::Day3Solver,
    day4::Day4Solver, day5::Day5Solver, day6::Day6Solver, day7::Day7Solver,
    day8::Day8Solver, day9::Day9Solver,
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
        5 => Box::new(Day5Solver),
        6 => Box::new(Day6Solver),
        7 => Box::new(Day7Solver),
        8 => Box::new(Day8Solver),
        9 => Box::new(Day9Solver),
        10 => Box::new(Day10Solver),
        11 => Box::new(Day11Solver),
        12 => Box::new(Day12Solver),
        13 => Box::new(Day13Solver),
        // Add new days here
        _ => panic!("Invalid day: {day}"),
    }
}
