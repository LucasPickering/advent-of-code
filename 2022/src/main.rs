mod days;
mod util;

use crate::days::{day1::Day1Solver, day2::Day2Solver, Solver};
use anyhow::bail;
use clap::Parser;
use std::fs;

/// Solvers for Advent of Code 2022
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Number of the day whose solver should be executed
    day: u8,

    /// Number of the part in the problem to solve (1 or 2)
    part: u8,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    let solver = get_solver(args.day);

    // Read input from the input path
    let path = format!("input/day{}.txt", args.day);
    eprintln!("Reading input from {path}");
    let input = fs::read_to_string(&path)?;

    // Run solver
    let output = match args.part {
        1 => solver.part1(input)?,
        2 => solver.part2(input)?,
        part => bail!("Invalid part: {part}"),
    };

    print!("{}", output);
    Ok(())
}

fn get_solver(day: u8) -> Box<dyn Solver> {
    match day {
        1 => Box::new(Day1Solver),
        2 => Box::new(Day2Solver),
        // Add new days here
        _ => panic!("Invalid day: {day}"),
    }
}
