mod day1;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;

/// A day's solvers. &self is needed so it can be a trait object
pub trait Solver {
    fn part1(&self, input: String) -> String;
    fn part2(&self, input: String) -> String;
}

pub fn get_solver(day: u8) -> Box<dyn Solver> {
    match day {
        1 => Box::new(day1::Solver),
        2 => Box::new(day2::Solver),
        3 => Box::new(day3::Solver),
        4 => Box::new(day4::Solver),
        5 => Box::new(day5::Solver),
        6 => Box::new(day6::Solver),
        7 => Box::new(day7::Solver),
        8 => Box::new(day8::Solver),
        9 => Box::new(day9::Solver),
        10 => Box::new(day10::Solver),
        11 => Box::new(day11::Solver),
        12 => Box::new(day12::Solver),
        13 => Box::new(day13::Solver),
        14 => Box::new(day14::Solver),
        15 => Box::new(day15::Solver),
        // Add new days here
        _ => panic!("Invalid day: {day}"),
    }
}
