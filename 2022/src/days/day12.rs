use crate::util::{Direction, Position};
use log::{debug, trace};
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

pub struct Solver;

impl super::Solver for Solver {
    fn part1(&self, input: String) -> String {
        let height_map = parse_input(&input);
        let searcher = Part1Searcher {
            start: height_map.start,
            end: height_map.end,
        };
        height_map.find_shortest_path(searcher).to_string()
    }

    fn part2(&self, input: String) -> String {
        let height_map = parse_input(&input);
        let searcher = Part2Searcher {
            // Go top-down, since this part has no hard-defined start
            start: height_map.end,
        };
        height_map.find_shortest_path(searcher).to_string()
    }
}

type Elevation = u8;

#[derive(Clone, Debug)]
struct HeightMap {
    /// Elevations are in the range [0, 25]. First is at position (0,0), in the
    /// top-left.
    elevations: Vec<Elevation>,
    width: usize,
    start: Position,
    end: Position,
}

#[derive(Clone, Debug)]
struct DistanceMap {
    width: isize,
    height: isize,
    distances: HashMap<Position, usize>,
}

/// An abstraction over the logic that varies between parts 1 and 2
trait Searcher {
    fn start(&self) -> Position;

    fn is_end(&self, position: Position, elevation: Elevation) -> bool;

    /// Can we step **from** `elevation1` **to** `elevation2`?
    fn can_step(&self, elevation1: Elevation, elevation2: Elevation) -> bool;
}

fn parse_input(input: &str) -> HeightMap {
    // All lines are the same length, so grab the first to get our width
    let width = input.lines().peekable().peek().unwrap().len();

    let mut elevations = Vec::with_capacity(input.len());
    let mut start = Position::default();
    let mut end = Position::default();
    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            match c {
                'S' => {
                    start = (x as isize, y as isize).into();
                    elevations.push(0);
                }
                'E' => {
                    end = (x as isize, y as isize).into();
                    elevations.push(25);
                }
                _ => elevations.push(c as Elevation - b'a'),
            }
        }
    }

    HeightMap {
        elevations,
        width,
        start,
        end,
    }
}

impl HeightMap {
    /// Get the **number of steps** required to get from start to end, via the
    /// shortest path
    fn find_shortest_path(&self, searcher: impl Searcher) -> usize {
        // We'll treat the grid like a graph, where there are edges between
        // neighbor nodes that are within 1 elevation of each other. Dijkstra's
        // to the rescue!
        // https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Algorithm

        let mut unvisited: HashSet<Position> = self.positions_iter().collect();
        let mut tentative_distances =
            DistanceMap::new(self.positions_iter(), searcher.start());

        while !unvisited.is_empty() {
            // Grab the most promising node. On first iteration, this will be
            // the start node
            let (current_node, current_distance) = unvisited
                .iter()
                .map(|position| {
                    (*position, tentative_distances.get(*position).unwrap())
                })
                .min_by_key(|(_, distance)| *distance)
                .unwrap();
            let current_elevation = self.get(current_node).unwrap();
            debug!(
                "Visiting node {current_node} (distance {current_distance})"
            );
            assert!(
                current_distance < usize::MAX,
                "Visiting node {current_node} with max distance - \
                    something went wrong. ({} nodes unvisited)\n{}",
                unvisited.len(),
                tentative_distances
            );

            // If we've reached the end, we're done :)
            if searcher.is_end(current_node, current_elevation) {
                return current_distance;
            }

            // Visit each unvisited traversable neighbor
            for (neighbor, _) in self
                .neighbors_iter(current_node)
                .filter(|(_, neighbor_elevation)| {
                    searcher.can_step(current_elevation, *neighbor_elevation)
                })
                .filter(|(neighbor, _)| unvisited.contains(neighbor))
            {
                trace!("  Checking neighbor {neighbor}");
                // If a tile is reachable, it's always 1 step away from its
                // neighbor. Update tentative distance to be the min of current
                // distance and new one
                tentative_distances
                    .set_distance(neighbor, current_distance + 1);
            }

            unvisited.remove(&current_node);
        }

        unreachable!("Visited all nodes without reaching destination");
    }

    fn get(&self, position: Position) -> Option<Elevation> {
        // Do safe conversions so we don't blow up on negative positions
        let x: usize = position.x.try_into().ok()?;
        let y: usize = position.y.try_into().ok()?;
        self.elevations.get(y * self.width + x).copied()
    }

    fn positions_iter(&self) -> impl Iterator<Item = Position> + '_ {
        self.elevations_iter().map(|(position, _)| position)
    }

    /// Get an iterator over all positions and their associated elevation
    fn elevations_iter(
        &self,
    ) -> impl Iterator<Item = (Position, Elevation)> + '_ {
        self.elevations
            .iter()
            .enumerate()
            .map(|(i, elevation)| (self.index_to_position(i), *elevation))
    }

    /// Get a list of all neighborsof this node given node
    fn neighbors_iter(
        &self,
        position: Position,
    ) -> impl Iterator<Item = (Position, Elevation)> + '_ {
        Direction::all().into_iter().filter_map(move |direction| {
            let neighbor = position + direction;
            Some((neighbor, self.get(neighbor)?))
        })
    }

    fn index_to_position(&self, index: usize) -> Position {
        ((index % self.width) as isize, (index / self.width) as isize).into()
    }
}

impl DistanceMap {
    fn new(positions: impl Iterator<Item = Position>, start: Position) -> Self {
        let mut max_x = 0;
        let mut max_y = 0;
        let mut distances = HashMap::new();

        for position in positions {
            max_x = isize::max(max_x, position.x);
            max_y = isize::max(max_y, position.y);
            let distance = if position == start { 0 } else { usize::MAX };
            distances.insert(position, distance);
        }

        Self {
            width: max_x + 1,
            height: max_y + 1,
            distances,
        }
    }

    /// Get the distance for a node, **if** it's less than the current value
    fn set_distance(&mut self, position: Position, distance: usize) {
        self.distances.entry(position).and_modify(|current| {
            trace!("    Setting {position} to min of ({distance}, {current})");
            *current = usize::min(*current, distance);
        });
    }

    fn get(&self, position: Position) -> Option<usize> {
        self.distances.get(&position).copied()
    }
}

impl Display for DistanceMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Start at -1 to fill in the margins
        for y in -1..self.height {
            for x in -1..self.width {
                if x == -1 && y == -1 {
                    // Top-left cell
                    write!(f, "|   |")?;
                } else if x == -1 {
                    // Left margin
                    write!(f, "|{y:>3}|")?;
                } else if y == -1 {
                    // Top margin
                    write!(f, "{x:>3}|")?;
                } else {
                    // Regular cell
                    let distance = self.get((x, y).into()).unwrap();
                    write!(
                        f,
                        "{:>3} ",
                        if distance < usize::MAX {
                            distance.to_string()
                        } else {
                            "?".into()
                        }
                    )?;
                }
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

/// Search bottom-up to a specific end tile
#[derive(Copy, Clone, Debug)]
struct Part1Searcher {
    start: Position,
    end: Position,
}

impl Searcher for Part1Searcher {
    fn start(&self) -> Position {
        self.start
    }

    fn is_end(&self, position: Position, _: Elevation) -> bool {
        position == self.end
    }

    fn can_step(&self, elevation1: Elevation, elevation2: Elevation) -> bool {
        (elevation2 as isize) - (elevation1 as isize) <= 1
    }
}

/// Search top-down to any tile with elevation 0
#[derive(Copy, Clone, Debug)]
struct Part2Searcher {
    start: Position,
}

impl Searcher for Part2Searcher {
    fn start(&self) -> Position {
        self.start
    }

    fn is_end(&self, _: Position, elevation: Elevation) -> bool {
        elevation == 0
    }

    fn can_step(&self, elevation1: Elevation, elevation2: Elevation) -> bool {
        // Negate the logic, since the trail should actually go the other way
        (elevation1 as isize) - (elevation2 as isize) <= 1
    }
}
