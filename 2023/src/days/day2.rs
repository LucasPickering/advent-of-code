use std::str::FromStr;

pub struct Solver;

impl super::Solver for Solver {
    fn part1(&self, input: String) -> String {
        let games: Vec<Game> =
            input.lines().map(Game::from_str).try_collect().unwrap();
        // Find possible games
        games
            .into_iter()
            .filter(|game| game.pulls.iter().all(Pull::is_possible))
            .map(|game| game.id)
            .sum::<usize>()
            .to_string()
    }

    fn part2(&self, _input: String) -> String {
        todo!()
    }
}

struct Game {
    id: usize,
    pulls: Vec<Pull>,
}

struct Pull {
    red: usize,
    green: usize,
    blue: usize,
}

impl Pull {
    fn is_possible(&self) -> bool {
        const RED: usize = 12;
        const GREEN: usize = 13;
        const BLUE: usize = 14;
        self.red <= RED && self.green <= GREEN && self.blue <= BLUE
    }
}

impl FromStr for Game {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        todo!()
    }
}

impl FromStr for Pull {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        todo!()
    }
}
