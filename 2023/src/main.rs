mod days;

use anyhow::bail;
use clap::Parser;
use env_logger::{Env, Target};
use std::{
    fs,
    io::{self, Read},
    path::PathBuf,
};

/// Solvers for Advent of Code 2022
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Number of the day whose solver should be executed
    day: u8,

    /// Number of the part in the problem to solve (1 or 2)
    part: u8,

    /// Path to input file, or - for stdin. Defaults to `input/dayX.txt`, where
    /// X is the day number
    input_path: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    env_logger::Builder::from_env(Env::default().default_filter_or("info"))
        // Logging should look like regular print output
        .format_level(false)
        .format_timestamp(None)
        .format_indent(None)
        .format_module_path(false)
        .format_target(false)
        .target(Target::Stdout)
        .init();
    let args = Args::parse();
    let solver = days::get_solver(args.day);

    // Read input from the input path (or stdin)
    let input_path = args
        .input_path
        .unwrap_or_else(|| format!("input/day{}.txt", args.day).into());
    let input = if input_path.to_string_lossy() == "-" {
        eprintln!("Reading input from stdin");
        let mut input = String::new();
        io::stdin().read_to_string(&mut input)?;
        input
    } else {
        eprintln!("Reading input from {input_path:?}");
        fs::read_to_string(&input_path)?
    };

    // Run solver
    let output = match args.part {
        1 => solver.part1(input),
        2 => solver.part2(input),
        part => bail!("Invalid part: {part}"),
    };

    print!("{}", output);
    Ok(())
}
