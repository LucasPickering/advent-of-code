use crate::{
    days::Part,
    tui::Tui,
    util::{Grid, Point2, Vector2},
};
use std::collections::{HashMap, HashSet};

pub struct Solver {
    grid: Grid<Option<Frequency>>,
    antennas: HashMap<Frequency, Vec<Point2>>,
}

type Frequency = char;

impl super::Solver for Solver {
    fn new(input: String, _: Tui) -> Self {
        let grid = Grid::from_rows(input.lines().map(|line| {
            line.chars().map(|c| if c == '.' { None } else { Some(c) })
        }));
        let antennas = grid.iter().fold(
            HashMap::<_, Vec<_>>::new(),
            |mut acc, (point, cell)| {
                if let Some(antenna) = cell {
                    acc.entry(*antenna).or_default().push(point);
                }
                acc
            },
        );
        Self { grid, antennas }
    }

    fn part1(self: Box<Self>) -> String {
        // Iterate over each unique pair in each antenna group. Each pair
        // crates two antinodes (unless they go off the map). It's possible for
        // two different pairs to create the same antinode, so we need to dedupe
        let mut antinodes = HashSet::new();
        for antennas in self.antennas.values() {
            antinodes.extend(self.get_antinodes(Part::Part1, antennas));
        }
        antinodes.len().to_string()
    }

    fn part2(self: Box<Self>) -> String {
        let mut antinodes = HashSet::new();
        for antennas in self.antennas.values() {
            antinodes.extend(self.get_antinodes(Part::Part2, antennas));
        }
        antinodes.len().to_string()
    }
}

impl Solver {
    /// Get all antinodes created by this group of antennas
    fn get_antinodes<'a>(
        &'a self,
        part: Part,
        antennas: &'a [Point2],
    ) -> impl 'a + Iterator<Item = Point2> {
        // Iterate over each unique pair. Ordering doesn't matter, so we can
        // shorten the inner loop to avoid duplicates
        antennas.iter().enumerate().flat_map(move |(i, antenna1)| {
            antennas[i + 1..].iter().flat_map(move |antenna2| {
                let pair = (*antenna1, *antenna2);
                match part {
                    Part::Part1 => self.get_antinodes_p1(pair),
                    Part::Part2 => self.get_antinodes_p2(pair),
                }
            })
        })
    }

    fn get_antinodes_p1(
        &self,
        (antenna1, antenna2): (Point2, Point2),
    ) -> Box<dyn '_ + Iterator<Item = Point2>> {
        let diff = antenna1.displacement(antenna2);
        // If either of these operations results in underflow/overflow,
        // they'll return None. We also need to check if they're
        // inbounds
        let iter = [antenna1 - diff, antenna2 + diff]
            .into_iter()
            .flatten()
            .filter(|p| self.grid.is_valid(*p));
        Box::new(iter)
    }

    fn get_antinodes_p2(
        &self,
        (antenna1, antenna2): (Point2, Point2),
    ) -> Box<dyn '_ + Iterator<Item = Point2>> {
        let diff = antenna1.displacement(antenna2);
        // I'm being lazy and collecting into a vec
        let mut antinodes = Vec::new();
        for step in 0.. {
            let antinode1 = self.do_diff(antenna1, diff * -step);
            let antinode2 = self.do_diff(antenna2, diff * step);
            if antinode1.is_none() && antinode2.is_none() {
                // Both projections have gone off the grid - we're done
                break;
            }
            antinodes.extend([antinode1, antinode2].into_iter().flatten())
        }
        Box::new(antinodes.into_iter())
    }

    fn do_diff(&self, point: Point2, diff: Vector2) -> Option<Point2> {
        let sum = (point + diff)?;
        if self.grid.is_valid(sum) {
            Some(sum)
        } else {
            None
        }
    }
}
