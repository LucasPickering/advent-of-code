use crate::util;
use anyhow::anyhow;
use std::str::FromStr;

pub struct Solver;

impl super::Solver for Solver {
    fn part1(&self, input: String) -> String {
        let games: Vec<(Move, Move)> = input
            .lines()
            .map(|line| {
                // Col 1 is opponent move, col 2 is our move
                let columns = util::words(line);
                let their_move: Move = columns[0].parse()?;
                let my_move: Move = columns[1].parse()?;
                Ok((their_move, my_move))
            })
            .collect::<anyhow::Result<Vec<_>>>()
            .unwrap();
        let total_score = games.into_iter().map(calc_score).sum::<u32>();
        total_score.to_string()
    }

    fn part2(&self, input: String) -> String {
        let games: Vec<(Move, Move)> = input
            .lines()
            .map(|line| {
                // Col 1 is opponent move, col 2 is outcome
                let columns = util::words(line);
                let their_move: Move = columns[0].parse()?;
                let outcome: Outcome = columns[1].parse()?;
                let my_move = their_move.move_for_outcome(outcome);
                Ok((their_move, my_move))
            })
            .collect::<anyhow::Result<Vec<_>>>()
            .unwrap();
        let total_score = games.into_iter().map(calc_score).sum::<u32>();
        total_score.to_string()
    }
}

fn calc_score((their_move, my_move): (Move, Move)) -> u32 {
    let outcome = my_move.outcome(their_move);
    my_move.score() + outcome.score()
}

#[derive(Copy, Clone, Debug)]
enum Move {
    Rock,
    Paper,
    Scissors,
}

impl Move {
    fn score(self) -> u32 {
        match self {
            Self::Rock => 1,
            Self::Paper => 2,
            Self::Scissors => 3,
        }
    }

    /// What outcome do *I* get against the given opposing move?
    fn outcome(self, other: Self) -> Outcome {
        match (self, other) {
            (Self::Rock, Self::Rock) => Outcome::Tie,
            (Self::Rock, Self::Paper) => Outcome::Loss,
            (Self::Rock, Self::Scissors) => Outcome::Win,
            (Self::Paper, Self::Rock) => Outcome::Win,
            (Self::Paper, Self::Paper) => Outcome::Tie,
            (Self::Paper, Self::Scissors) => Outcome::Loss,
            (Self::Scissors, Self::Rock) => Outcome::Loss,
            (Self::Scissors, Self::Paper) => Outcome::Win,
            (Self::Scissors, Self::Scissors) => Outcome::Tie,
        }
    }

    /// What move should *I* use to get the intended outcome?
    fn move_for_outcome(self, outcome: Outcome) -> Move {
        match (self, outcome) {
            (Self::Rock, Outcome::Win) => Self::Paper,
            (Self::Rock, Outcome::Loss) => Self::Scissors,
            (Self::Rock, Outcome::Tie) => Self::Rock,
            (Self::Paper, Outcome::Win) => Self::Scissors,
            (Self::Paper, Outcome::Loss) => Self::Rock,
            (Self::Paper, Outcome::Tie) => Self::Paper,
            (Self::Scissors, Outcome::Win) => Self::Rock,
            (Self::Scissors, Outcome::Loss) => Self::Paper,
            (Self::Scissors, Outcome::Tie) => Self::Scissors,
        }
    }
}

impl FromStr for Move {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A" | "X" => Ok(Self::Rock),
            "B" | "Y" => Ok(Self::Paper),
            "C" | "Z" => Ok(Self::Scissors),
            other => Err(anyhow!("Invalid move: {other}")),
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum Outcome {
    Win,
    Loss,
    Tie,
}

impl Outcome {
    fn score(self) -> u32 {
        match self {
            Self::Win => 6,
            Self::Loss => 0,
            Self::Tie => 3,
        }
    }
}

impl FromStr for Outcome {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "X" => Ok(Self::Loss),
            "Y" => Ok(Self::Tie),
            "Z" => Ok(Self::Win),
            other => Err(anyhow!("Invalid output: {other}")),
        }
    }
}
