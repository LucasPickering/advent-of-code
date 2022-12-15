use crate::util::{CapturesExt, MatchExt, Position};
use derive_more::Display;
use lazy_static::lazy_static;
use log::{debug, info, trace};
use regex::Regex;
use std::{
    cmp,
    collections::{HashMap, HashSet},
    fmt::Display,
};

pub struct Solver;

impl super::Solver for Solver {
    fn part1(&self, input: String) -> String {
        let cave = parse_input(&input);
        info!(
            "Parsed {} sensors and {} beacons",
            cave.sensors.len(),
            cave.beacons.len()
        );
        let y = 2_000_000;
        let precluded_ranges = cave.get_precluded_ranges(y, None);
        let num_known_beacons =
            cave.beacons.iter().filter(|beacon| beacon.y == y).count();
        (precluded_ranges.len() - num_known_beacons).to_string()
    }

    fn part2(&self, input: String) -> String {
        let cave = parse_input(&input);
        let hidden_beacon = cave.find_hidden_beacon(Rectangle {
            min: (0, 0).into(),
            max: (4_000_000, 4_000_000).into(),
        });
        info!("Hidden beacon is at {}", hidden_beacon);
        hidden_beacon.tuning_frequency().to_string()
    }
}

#[derive(Clone, Debug)]
struct Cave {
    // A map of sensors, each mapped to its nearest beacon
    sensors: HashMap<Position, Position>,
    // List of all *known* beacons
    beacons: HashSet<Position>,
}

struct Rectangle {
    min: Position,
    max: Position,
}

fn parse_input(input: &str) -> Cave {
    lazy_static! {
        static ref SENSOR_RGX: Regex = Regex::new(r"Sensor at (.*):").unwrap();
        static ref BEACON_RGX: Regex =
            Regex::new(r"beacon is at (.*)$").unwrap();
    }

    let mut sensors = HashMap::new();
    let mut beacons = HashSet::new();
    for line in input.lines() {
        // For both sensor and beacon, isolate the `x=.., y=..` part and parse
        // that into a Position
        let sensor: Position = SENSOR_RGX
            .captures(line)
            .unwrap()
            .get_unwrap(1)
            .parse_unwrap();
        let beacon: Position = BEACON_RGX
            .captures(line)
            .unwrap()
            .get_unwrap(1)
            .parse_unwrap();
        sensors.insert(sensor, beacon);
        beacons.insert(beacon);
    }

    Cave { sensors, beacons }
}

impl Cave {
    /// Count the number of unknown positions in a given row
    fn get_precluded_ranges(
        &self,
        y: isize,
        x_bounds: Option<(isize, isize)>,
    ) -> Ranges {
        let mut non_beacon_ranges = Ranges::new();

        for (&sensor, &nearest_beacon) in &self.sensors {
            // The distance between the sensor and its nearest beacon tells us
            // how big of an area *doesn't* contain a beacon, aside from the
            // one know beacon at the edge of that area.
            let radius = sensor.manhattan_distance(nearest_beacon);

            // For a single row, the beaconless zone has a fixed width, which
            // is inversely proportional to its distance from the sensor.
            // If the sensor is *on* the row, it's 2r+1, decaying down by the
            // function w=2(r-d)+1. If d>r, then this sensor tells us nothing
            // and we can ignore it.
            let y_distance = (sensor.y - y).abs();
            let half_width = radius - y_distance;

            // This sensor sucks and we hate it. Useless piece of shit.
            if half_width < 0 {
                continue;
            }

            let mut x_min = sensor.x - half_width;
            let mut x_max = sensor.x + half_width;
            // Don't let the range bounds be outside the absolute x bounds
            if let Some(x_bounds) = x_bounds {
                x_min = cmp::max(x_min, x_bounds.0);
                x_max = cmp::min(x_max, x_bounds.1);
            }
            non_beacon_ranges.add(x_min, x_max);
        }

        non_beacon_ranges
    }

    /// Find the one position within the search region that isn't precluded from
    /// having a beacon.
    fn find_hidden_beacon(&self, search_region: Rectangle) -> Position {
        let x_bounds = (search_region.min.x, search_region.max.x);
        let cells_per_row = (x_bounds.1 - x_bounds.0 + 1) as usize;
        for y in search_region.min.y..=search_region.max.y {
            if y % 1000 == 0 {
                debug!("Checking row {y}");
            }

            let precluded_ranges = self.get_precluded_ranges(y, Some(x_bounds));
            if precluded_ranges.len() < cells_per_row {
                // We found the row with the beacon, let's search it now
                for x in search_region.min.x..=search_region.max.x {
                    if !precluded_ranges.contains(x) {
                        return (x, y).into();
                    }
                }
            }
        }

        panic!("Couldn't find any viable hidden beacon locations!");
    }
}

/// A collection of numeric ranges. The contained ranges are all guaranteed to
/// be disjoint with each other. When a new range is added, it gets de-duped if
/// possible.
#[derive(Clone, Debug)]
struct Ranges {
    /// This should always be sorted by min, and ranges are guaranteed to be
    /// disjoint. This means it will inherently be sorted by max too.
    ranges: Vec<Range>,
}

impl Ranges {
    fn new() -> Self {
        Self { ranges: Vec::new() }
    }

    fn add(&mut self, min: isize, max: isize) {
        assert!(min <= max, "min must be <= max");
        trace!("Adding range [{min}, {max}] to {self}");

        // Walk through the list of existing ranges, and join as many of them
        // as possible
        let mut range = Range::new(min, max);
        let mut index = 0;
        while index < self.ranges.len() {
            let other_range = &self.ranges[index];
            if range.union(other_range.min, other_range.max) {
                // Since we sucked up this range, remove it then repeat on this
                // index
                self.ranges.remove(index);
            } else {
                // No interaction here, just move on
                index += 1;
            }
        }

        // We've reduced the array as much as possible now, let's insert the
        // new range into it, maintaining the sort
        match self.ranges.binary_search_by_key(&min, |range| range.min) {
            Ok(index) => {
                panic!("Collided with range at index {index}, we should've joined it!");
            }
            Err(index) => {
                self.ranges.insert(index, range);
            }
        }
        trace!("  Result: {self}");
    }

    fn len(&self) -> usize {
        self.ranges.iter().map(Range::len).sum()
    }

    fn contains(&self, value: isize) -> bool {
        self.ranges.iter().any(|range| range.contains(value))
    }
}

impl Display for Ranges {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;

        for (i, range) in self.ranges.iter().enumerate() {
            if i > 0 {
                write!(f, " | ")?;
            }
            write!(f, "{}", range)?;
        }

        write!(f, "}}")?;
        Ok(())
    }
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "[{}, {}]", min, max)]
struct Range {
    min: isize,
    max: isize,
}

impl Range {
    fn new(min: isize, max: isize) -> Self {
        assert!(min <= max, "min must be <= max");
        Self { min, max }
    }

    /// If this range touches the given range at all, mutate this range to
    /// include the new one and return true. If the two are disjoint, do nothing
    /// and return false.
    fn union(&mut self, min: isize, max: isize) -> bool {
        assert!(min <= max, "min must be <= max");
        // First check if it's disjoint. There are two possible cases here:
        // - New range is entirely below us
        // - New range is entirely above us
        if self.touches(min, max) {
            // We know the two ranges touch somehow, so let's take the widest
            // bounds possible
            self.min = cmp::min(self.min, min);
            self.max = cmp::max(self.max, max);
            true
        } else {
            false
        }
    }

    fn touches(&self, min: isize, max: isize) -> bool {
        // We need to adjust by 1 to account for adjacent non-overlapping ranges
        !(max + 1 < self.min || self.max < min - 1)
    }

    fn len(&self) -> usize {
        // +1 for inclusive bounds. Cast is safe since we know min <= max
        (self.max - self.min + 1) as usize
    }

    fn contains(&self, value: isize) -> bool {
        self.min <= value && value <= self.max
    }
}

impl Position {
    fn tuning_frequency(self) -> isize {
        self.x * 4_000_000 + self.y
    }
}
