use crate::util::{self, Direction, Position, PositionIterator};
use derive_more::Display;
use itertools::Itertools;
use log::{debug, info};
use std::{collections::HashMap, fmt::Display};

pub struct Solver;

impl super::Solver for Solver {
    fn part1(&self, input: String) -> String {
        let mut cave = parse_input(&input);
        info!("{cave}");
        while cave.add_sand(true) {
            debug!("{cave}");
        }
        info!("{cave}");
        cave.count_sand().to_string()
    }

    fn part2(&self, input: String) -> String {
        let mut cave = parse_input(&input);
        cave.add_floor();
        info!("{cave}");
        while cave.cells.get(&cave.spawn_position).copied() != Some(Cell::Sand)
        {
            cave.add_sand(false);
            debug!("{cave}");
        }
        info!("{cave}");
        cave.count_sand().to_string()
    }
}

#[derive(Clone, Debug)]
struct Cave {
    cells: HashMap<Position, Cell>,
    spawn_position: Position,
}

#[derive(Copy, Clone, Debug, Display, Eq, PartialEq)]
enum Cell {
    #[display(fmt = "#")]
    Rock,
    #[display(fmt = ".")]
    Air,
    #[display(fmt = "o")]
    Sand,
    #[display(fmt = "+")]
    Spawn,
}

fn parse_input(input: &str) -> Cave {
    fn parse_path(line: &str) -> Vec<Position> {
        line.split(" -> ")
            .map(str::parse)
            .collect::<Result<Vec<_>, _>>()
            .unwrap()
    }

    let mut cells = HashMap::new();
    // Trace the paths to add rock
    for path in input.lines().map(parse_path) {
        for (start, end) in path.into_iter().tuple_windows() {
            // Make sure start <= end for both x and y, otherwise iteration
            // won't do anything
            let (start_x, end_x) = util::min_max(start.x, end.x);
            let (start_y, end_y) = util::min_max(start.y, end.y);

            // Start and end should fall on a horizontal/vertical line, meaning
            // they should have the same x OR the same Y.
            for (x, y) in (start_x..=end_x)
                .into_iter()
                .cartesian_product(start_y..=end_y)
            {
                cells.insert((x, y).into(), Cell::Rock);
            }
        }
    }

    Cave::new(cells)
}

impl Cave {
    fn new(mut cells: HashMap<Position, Cell>) -> Self {
        let spawn_position = (500, 0).into();
        cells.insert(spawn_position, Cell::Spawn);
        Self {
            cells,
            spawn_position,
        }
    }

    /// Get the cell at a given position. Returns Air for any unknown cell.
    fn get(&self, position: Position) -> Cell {
        self.cells.get(&position).copied().unwrap_or(Cell::Air)
    }

    /// Add an "infinite" rock floor to the bottom of the cave
    fn add_floor(&mut self) {
        let (min_x, _, max_x, max_y) =
            self.cells.keys().copied().bounds().unwrap();
        let floor_y = max_y + 2;
        let buffer_size = max_y + 2;
        // "Infinite" in this case means "just wide enough"
        for x in (min_x - buffer_size)..=(max_x + buffer_size) {
            self.cells.insert((x, floor_y).into(), Cell::Rock);
        }
    }

    /// Add a single sand to the cave and simulate it falling. Returns true if
    /// the sand settled, false if it falls into the void
    fn add_sand(&mut self, check_for_void: bool) -> bool {
        let mut sand_position = self.spawn_position;

        // Technically we only have to check has_below whenever the x value of
        // sand_position changes, but this is fast enough anyway
        while !check_for_void || self.has_below(sand_position) {
            // Down is up when you're underground :)
            let below = sand_position + Direction::Up;
            let below_left = below + Direction::Left;
            let below_right = below + Direction::Right;

            // Try to move the sand straight down, then down-left, then
            // down-right.
            if !self.get(below).is_solid() {
                sand_position = below;
            } else if !self.get(below_left).is_solid() {
                sand_position = below_left;
            } else if !self.get(below_right).is_solid() {
                sand_position = below_right;
            } else {
                // Sand can't move, we're done
                self.cells.insert(sand_position, Cell::Sand);
                return true;
            }
        }

        // The sand started falling off into the void
        false
    }

    /// Does the cave have *anything solid* below the given position?
    fn has_below(&self, position: Position) -> bool {
        self.cells.iter().any(|(cell_position, cell)| {
            cell_position.x == position.x
            // >y is lower down in this universe
            && cell_position.y > position.y
            // There *shouldn't* be any air stored in the cave, but be safe
                && cell.is_solid()
        })
    }

    /// Get the number of sand cells in the save
    fn count_sand(&self) -> usize {
        self.cells
            .values()
            .filter(|cell| **cell == Cell::Sand)
            .count()
    }
}

impl Display for Cave {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (min_x, min_y, max_x, max_y) =
            self.cells.keys().copied().bounds().unwrap();
        for y in min_y..=max_y {
            for x in min_x..=max_x {
                write!(f, "{}", self.get((x, y).into()))?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Cell {
    fn is_solid(self) -> bool {
        match self {
            Self::Air => false,
            Self::Rock | Self::Sand | Self::Spawn => true,
        }
    }
}
