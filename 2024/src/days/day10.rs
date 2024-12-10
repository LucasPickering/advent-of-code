use crate::{
    tui::Tui,
    util::{Grid, Point2},
};
use std::{collections::HashSet, iter};

pub struct Solver {
    grid: Grid<Height>,
}

type Height = u32;

type Score = u32;

impl super::Solver for Solver {
    fn new(input: String, _: Tui) -> Self {
        let grid = Grid::from_rows(
            input
                .lines()
                .map(|line| line.chars().filter_map(|c| c.to_digit(10))),
        );
        Self { grid }
    }

    fn part1(self: Box<Self>) -> String {
        let trailheads = self.find_trailheads();
        trailheads
            .map(|th| self.get_score(th))
            .sum::<Score>()
            .to_string()
    }

    fn part2(self: Box<Self>) -> String {
        let trailheads = self.find_trailheads();
        trailheads
            .map(|th| self.get_rating(th))
            .sum::<Score>()
            .to_string()
    }
}

impl Solver {
    fn find_trailheads(&self) -> impl '_ + Iterator<Item = Point2> {
        self.grid
            .iter()
            .filter(|(_, height)| **height == 0)
            .map(|(p, _)| p)
    }

    fn get_score(&self, trailhead: Point2) -> Score {
        self.get_summits(trailhead).collect::<HashSet<_>>().len() as Score
    }

    /// Get all summits reachable from a given point, **with potential
    /// duplicates**
    fn get_summits(
        &self,
        point: Point2,
    ) -> Box<dyn '_ + Iterator<Item = Point2>> {
        let height = self.grid[point];
        // Base case
        if height == 9 {
            return Box::new(iter::once(point));
        }

        // BFS
        let iter = self
            .grid
            .adjacents(point)
            .filter(move |(_, adj_height)| **adj_height == height + 1)
            .flat_map(|(adj_point, _)| self.get_summits(adj_point));
        // We have to box the iterator because the recursion makes it impossible
        // to give it a static type
        Box::new(iter)
    }

    fn get_rating(&self, trailhead: Point2) -> Score {
        // The P1 algorithm already works, because it is actually counting each
        // unique way to reach each summit, so summits with multiple approach
        // trails are counted multiple times. A convenient surprise :)
        self.get_summits(trailhead).count() as Score
    }
}
