mod day1;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day17;
mod day18;
mod day19;
mod day2;
mod day20;
mod day21;
mod day22;
mod day23;
mod day24;
mod day25;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;

/// A day's solvers
pub trait Solver {
    /// Parse input and initialize the solver
    fn new(input: String) -> Self
    where
        Self: Sized;

    /// Solve part 1
    fn part1(self: Box<Self>) -> String;

    /// Solve part 2
    fn part2(self: Box<Self>) -> String;
}

pub fn get_solver(day: u8, input: String) -> Box<dyn Solver> {
    match day {
        1 => Box::new(day1::Solver::new(input)),
        2 => Box::new(day2::Solver::new(input)),
        3 => Box::new(day3::Solver::new(input)),
        4 => Box::new(day4::Solver::new(input)),
        5 => Box::new(day5::Solver::new(input)),
        6 => Box::new(day6::Solver::new(input)),
        7 => Box::new(day7::Solver::new(input)),
        8 => Box::new(day8::Solver::new(input)),
        9 => Box::new(day9::Solver::new(input)),
        10 => Box::new(day10::Solver::new(input)),
        11 => Box::new(day11::Solver::new(input)),
        12 => Box::new(day12::Solver::new(input)),
        13 => Box::new(day13::Solver::new(input)),
        14 => Box::new(day14::Solver::new(input)),
        15 => Box::new(day15::Solver::new(input)),
        16 => Box::new(day16::Solver::new(input)),
        17 => Box::new(day17::Solver::new(input)),
        18 => Box::new(day18::Solver::new(input)),
        19 => Box::new(day19::Solver::new(input)),
        20 => Box::new(day20::Solver::new(input)),
        21 => Box::new(day21::Solver::new(input)),
        22 => Box::new(day22::Solver::new(input)),
        23 => Box::new(day23::Solver::new(input)),
        24 => Box::new(day24::Solver::new(input)),
        25 => Box::new(day25::Solver::new(input)),
        // Add new days here
        _ => panic!("Invalid day: {day}"),
    }
}
