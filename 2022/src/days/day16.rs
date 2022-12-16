use crate::util::{CapturesExt, MatchExt};
use derive_more::Display;
use itertools::Itertools;
use lazy_static::lazy_static;
use log::{debug, info, trace};
use regex::Regex;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::Hash,
};

pub struct Solver;

impl super::Solver for Solver {
    fn part1(&self, input: String) -> String {
        let scan = parse_input(&input);
        info!("{scan}");
        scan.find_most_release_path("AA", 30).to_string()
    }

    fn part2(&self, _input: String) -> String {
        todo!()
    }
}

#[derive(Clone, Debug)]
struct ValveScan {
    valves: HashMap<String, Valve>,
}

#[derive(Clone, Debug, Display)]
#[display(fmt = "{}", label)]
struct Valve {
    label: String,
    flow_rate: usize,
    tunnels: Vec<String>,
}

fn parse_input(input: &str) -> ValveScan {
    lazy_static! {
        static ref VALVE_REGEX: Regex = Regex::new(
            r"^Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)$"
        )
        .unwrap();
    }

    let valves = input
        .lines()
        .map(|line| {
            let caps = VALVE_REGEX.captures(line).unwrap();
            let label = caps.get_unwrap(1).as_str().to_owned();
            let flow_rate = caps.get_unwrap(2).parse_unwrap();
            let tunnels = caps
                .get_unwrap(3)
                .as_str()
                .split(", ")
                .map(str::to_owned)
                .collect();
            (
                label.clone(),
                Valve {
                    label,
                    flow_rate,
                    tunnels,
                },
            )
        })
        .collect();
    ValveScan { valves }
}

#[derive(Copy, Clone, Debug)]
struct Path<'a> {
    start: &'a Valve,
    end: &'a Valve,
    distance: usize,
}

type PathCache<'a> = HashMap<(&'a str, &'a str), Path<'a>>;

impl ValveScan {
    /// Find the valve path that releases the most pressure
    fn find_most_release_path<'a>(
        &'a self,
        start: &'a str,
        minutes: usize,
    ) -> usize {
        // A cache of the shortest path between each node. The key will be
        // sorted alphabetically, and the path will be from the first to the
        // second
        let mut paths: PathCache = HashMap::new();
        let mut unopened_valves: HashSet<&Valve> = self
            .valves
            .values()
            // We'd never want to open rate=0 valves, so don't consider them
            .filter(|valve| valve.flow_rate > 0)
            .collect();

        let mut current_valve = self.valves.get(start).unwrap();
        let mut total_released = 0;
        let mut released_per_minute = 0;
        let mut remaining_minutes = minutes;

        while remaining_minutes > 0 {
            // Rank each unopened valve by its (flow rate - distance), and pick
            // the best
            let next_valve = unopened_valves
                .iter()
                .map(|&other_valve| {
                    let path =
                        self.get_path_to_uncached(current_valve, other_valve);
                    (other_valve, path.distance)
                })
                // Exclude any valve that is too far away to open in time. We
                // need n minutes to get there, plus 1 more minute to open it,
                // plus another minute to start getting results. So if there's
                // less than n+2 minutes left, no point in trying to open it
                .filter(|(_, num_steps)| remaining_minutes >= num_steps + 2)
                .max_by_key(|(valve, num_steps)| {
                    valve.flow_rate as isize - *num_steps as isize
                });

            match next_valve {
                // There's still work to be done...
                Some((next_valve, steps)) => {
                    // Simulating movement to the new valve
                    remaining_minutes -= steps;
                    total_released += released_per_minute * steps;

                    // Open the valve
                    debug!("Opening valve {next_valve}");
                    released_per_minute += next_valve.flow_rate;
                    unopened_valves.remove(next_valve);
                    current_valve = next_valve;
                }
                // If we've already opened all the valves, or there isn't time
                // left to open any more, we'll just sit around and wait. That
                // means we can just skip to the end
                None => {
                    debug!("Skipping final {remaining_minutes} minutes");
                    total_released += released_per_minute * remaining_minutes;
                    break;
                }
            }
        }

        total_released
    }

    // Get the shortest path between two nodes in the graph, updating the cache
    // in the process. The returned path **will not include the start or end**
    // fn get_path_to<'a>(
    //     &'a self,
    //     path_cache: &'a mut PathCache<'a>,
    //     start: &'a Valve,
    //     end: &'a Valve,
    // ) -> &'a Path {
    //     // Cache keys are canonically sorted alphabetically, because the
    // graph     // is symmetrical
    //     let cache_key: (&str, &str) = if start.label < end.label {
    //         (&start.label, &end.label)
    //     } else {
    //         (&end.label, &start.label)
    //     };

    //     if let Some(path) = path_cache.get(&cache_key) {
    //         trace!("HIT {} -> {}", start.label, end.label);
    //         path
    //     } else {
    //         trace!("MISS {} -> {}", start.label, end.label);

    //         let path = self.get_path_to_uncached(start, end);

    //         path_cache.insert(cache_key, path);
    //         // TODO get rid of ugly unwrap
    //         path_cache.get(&cache_key).unwrap()
    //     }
    // }

    fn get_path_to_uncached<'a>(
        &'a self,
        start: &'a Valve,
        end: &'a Valve,
    ) -> Path<'a> {
        // We gotta do some dijkstra's now
        let mut unvisited: HashSet<&str> =
            self.valves.keys().map(String::as_str).collect();
        let mut tentative_distances: HashMap<&str, usize> = self
            .valves
            .keys()
            .map(|label| {
                let initial_distance =
                    if label == &start.label { 0 } else { usize::MAX };
                (label.as_str(), initial_distance)
            })
            .collect();
        debug!("Finding path {start} -> {end}");

        while !unvisited.is_empty() {
            // Grab the most promising unvisited node
            let (current_label, &current_distance) = unvisited
                .iter()
                .map(|label| (*label, tentative_distances.get(*label).unwrap()))
                .min_by_key(|(_, distance)| *distance)
                .unwrap();
            let current_valve = &self.valves[current_label];

            if current_valve == end {
                debug!("{start} -> {end} is {} steps", current_distance);
                return Path {
                    start,
                    end,
                    distance: current_distance,
                };
            }

            // Visit each unvisited neighbor
            for neighbor in current_valve
                .tunnels
                .iter()
                .filter(|neighbor| unvisited.contains(neighbor.as_str()))
            {
                // If a tile is reachable, it's always 1 step away from its
                // neighbor. Update tentative distance to be the min of current
                // distance and new one
                tentative_distances.entry(neighbor).and_modify(|current| {
                    *current = usize::min(*current, current_distance + 1)
                });
            }

            unvisited.remove(current_label);
        }

        unreachable!(
            "Failed to find optimal path from {} to {}",
            start.label, end.label
        );
    }
}

impl Display for ValveScan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for valve in self.valves.values().sorted_by_key(|valve| &valve.label) {
            write!(
                f,
                "{} {:>2} => {}",
                valve.label,
                valve.flow_rate,
                valve.tunnels.join(", ")
            )?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl PartialEq for Valve {
    fn eq(&self, other: &Self) -> bool {
        self.label == other.label
    }
}

impl Eq for Valve {}

impl Hash for Valve {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.label.hash(state);
    }
}
