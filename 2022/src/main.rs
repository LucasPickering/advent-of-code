mod days;

use crate::days::{day1::Day1Solver, Solver};
use clap::Parser;
use std::io::{self, Read};

/// Solvers for Advent of Code 2022
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Number of the day whose solver should be executed
    day: u8,

    /// Number of the part in the problem to solve (1 or 2)
    part: u8,
}

fn main() {
    let args = Args::parse();
    let solver = get_solver(args.day);

    // Read input from stdin (read to EOF)
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    // Run solver
    let output = match args.part {
        1 => solver.part1(input),
        2 => solver.part2(input),
        part => panic!("Invalid part: {part}"),
    };

    print!("{}", output);
}

fn get_solver(day: u8) -> Box<dyn Solver> {
    match day {
        1 => Box::new(Day1Solver),
        _ => panic!("Invalid day: {day}"),
    }
}
