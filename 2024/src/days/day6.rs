use crate::{
    tui::{Level, Tui},
    util::{Direction, Point2},
};
use ratatui::{
    buffer::Buffer,
    layout::Rect,
    style::{Color, Modifier, Style},
    text::{Line, Span, Text},
    widgets::{Paragraph, Widget},
};
use std::collections::{HashMap, HashSet};

pub struct Solver {
    /// y:obstructions in that row, sorted ascending
    rows: HashMap<usize, Vec<usize>>,
    /// x:obstructions in that column, sorted ascending
    columns: HashMap<usize, Vec<usize>>,
    width: usize,
    height: usize,
    guard: Guard,
    tui: Tui,
}

#[derive(Copy, Clone)]
struct Guard {
    position: Point2,
    direction: Direction,
}

struct Simulation {
    guard: Guard,
    /// y:obstructions in that row, sorted ascending
    rows: HashMap<usize, Vec<usize>>,
    /// x:obstructions in that column, sorted ascending
    columns: HashMap<usize, Vec<usize>>,
    width: usize,
    height: usize,
    visited: HashSet<(Point2, Option<Direction>)>,
}

impl super::Solver for Solver {
    fn new(input: String, tui: Tui) -> Self {
        let mut rows: HashMap<usize, Vec<usize>> = HashMap::new();
        let mut columns: HashMap<usize, Vec<usize>> = HashMap::new();
        let mut guard = None;
        for (y, line) in input.lines().enumerate() {
            for (x, c) in line.chars().enumerate() {
                match c {
                    '#' => {
                        rows.entry(y).or_default().push(x);
                        columns.entry(x).or_default().push(y);
                    }
                    '^' => {
                        guard = Some(Guard {
                            position: Point2 { x, y },
                            direction: Direction::Up,
                        })
                    }
                    '.' => {}
                    _ => panic!("Unexpected char: {c}"),
                }
            }
        }

        // Input has a collider in the rightmost column and bottommost row
        let width = columns.keys().max().copied().unwrap_or_default() + 1;
        let height = rows.keys().max().copied().unwrap_or_default() + 1;

        Self {
            rows,
            columns,
            guard: guard.expect("guard not found"),
            width,
            height,
            tui,
        }
    }

    fn part1(mut self: Box<Self>) -> String {
        let mut visited = HashSet::new();
        let mut sim = Simulation {
            guard: self.guard,
            rows: self.rows,
            columns: self.columns,
            width: self.width,
            height: self.height,
            visited: HashSet::new(),
        };
        self.tui.show(Level::Info, &sim);
        loop {
            let (new_visited, collided) = sim.move_guard();
            visited.extend(new_visited);
            self.tui.show(Level::Info, &sim);
            if !collided {
                break;
            }
        }
        visited.len().to_string()
    }

    fn part2(self: Box<Self>) -> String {
        // Brute force by adding an obstacle to every position that doesn't
        // have one, and running a sim to see if there's a loop
        let mut loop_obstacles: HashSet<Point2> = HashSet::new();
        let max_sims = self.width * self.height;
        for y in 0..self.height {
            for x in 0..self.width {
                let i = (y * self.width + x) + 1;
                if i % 100 == 0 {
                    println!("{}/{}", i, max_sims);
                }
                // If there isn't already an obstacle here, try adding one
                if !self.rows[&y].contains(&x) {
                    let mut rows = self.rows.clone();
                    insert(rows.entry(y).or_default(), x);
                    let mut columns = self.columns.clone();
                    insert(columns.entry(x).or_default(), y);
                    let mut sim = Simulation {
                        guard: self.guard,
                        rows,
                        columns,
                        width: self.width,
                        height: self.height,
                        visited: HashSet::new(),
                    };

                    let obstacle = Point2 { x, y };
                    if let Outcome::Loop = sim.run_p2() {
                        loop_obstacles.insert(obstacle);
                    }
                }
            }
        }
        loop_obstacles.len().to_string()
    }
}

impl Simulation {
    fn run_p2(&mut self) -> Outcome {
        let mut visited: HashSet<(Point2, Direction)> = HashSet::new();
        loop {
            let direction = self.guard.direction;
            let (new_visited, collided) = self.move_guard();
            for p in new_visited {
                if !visited.insert((p, direction)) {
                    return Outcome::Loop;
                }
            }
            if !collided {
                break;
            }
        }
        Outcome::Exit
    }

    /// Move the guard forward until they hit an obstruction or exit the map.
    /// Return `true` if the guard is still roaming (hit an obstruction),
    /// `false` if they left the map
    fn move_guard(&mut self) -> (Box<dyn Iterator<Item = Point2>>, bool) {
        let guard = self.guard;
        let max_x = self.width - 1;
        let max_y = self.height - 1;

        let (new_position, visited): (
            Point2,
            Box<dyn Iterator<Item = Point2>>,
        ) = match guard.direction {
            Direction::Up => {
                let x = guard.position.x;
                let column = self.column(x);
                let new_y = minimize(column, guard.position.y).unwrap_or(0);
                let visited =
                    (new_y..=guard.position.y).map(move |y| Point2 { x, y });
                (Point2 { x, y: new_y }, Box::new(visited))
            }
            Direction::Down => {
                let x = guard.position.x;
                let column = self.column(x);
                let new_y = maximize(column, guard.position.y).unwrap_or(max_y);
                let visited =
                    (guard.position.y..=new_y).map(move |y| Point2 { x, y });
                (Point2 { x, y: new_y }, Box::new(visited))
            }
            Direction::Left => {
                let y = guard.position.y;
                let row = self.row(y);
                let new_x = minimize(row, guard.position.x).unwrap_or(0);
                let visited =
                    (new_x..=guard.position.x).map(move |x| Point2 { x, y });
                (Point2 { x: new_x, y }, Box::new(visited))
            }
            Direction::Right => {
                let y = guard.position.y;
                let row = self.row(y);
                let new_x = maximize(row, guard.position.x).unwrap_or(max_x);
                let visited =
                    (guard.position.x..=new_x).map(move |x| Point2 { x, y });
                (Point2 { x: new_x, y }, Box::new(visited))
            }
        };
        let Point2 { x, y } = new_position;
        let collided = 0 < x && x < max_x && 0 < y && y < max_y;
        self.guard = Guard {
            position: new_position,
            direction: guard.direction.clockwise(),
        };

        (visited, collided)
    }

    fn row(&self, y: usize) -> &[usize] {
        self.rows.get(&y).map(Vec::as_slice).unwrap_or_default()
    }

    fn column(&self, x: usize) -> &[usize] {
        self.columns.get(&x).map(Vec::as_slice).unwrap_or_default()
    }
}

/// Reduce a value until it would cross over the next value in a series of
/// bounds
fn minimize(bounds: &[usize], value: usize) -> Option<usize> {
    assert!(bounds.is_sorted());
    let index = bounds.partition_point(|bound| *bound < value);
    if index > 0 {
        // The index is for the bound _past_ the value, so we want to be just
        // after the bound _before_ that
        Some(bounds[index - 1] + 1)
    } else {
        // We're already below all bounds
        None
    }
}

/// Increase a value until it would cross over the next value in a series of
/// bounds
fn maximize(bounds: &[usize], value: usize) -> Option<usize> {
    assert!(bounds.is_sorted());
    let index = bounds.partition_point(|bound| *bound < value);
    if index < bounds.len() {
        // Index is for the bound past the value, get one step before that
        Some(bounds[index] - 1)
    } else {
        // We're already above all bounds
        None
    }
}

fn insert(v: &mut Vec<usize>, value: usize) {
    let Err(index) = v.binary_search(&value) else {
        panic!("can't insert {value} into {v:?}: already there")
    };
    v.insert(index, value);
}

enum Outcome {
    Loop,
    Exit,
}

impl Widget for &Simulation {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let lines = (0..self.height).map(|y| {
            (0..self.width)
                .map(|x| {
                    let point = Point2 { x, y };
                    if self.guard.position == point {
                        let c = match self.guard.direction {
                            Direction::Up => "^",
                            Direction::Right => ">",
                            Direction::Down => "v",
                            Direction::Left => "<",
                        };
                        Span::styled(
                            c,
                            Style::new()
                                .fg(Color::Red)
                                .add_modifier(Modifier::BOLD),
                        )
                    } else if self.visited.contains(&(point, None)) {
                        "X".into()
                    } else if self
                        .rows
                        .get(&y)
                        .map(|row| row.binary_search(&x).is_ok())
                        .unwrap_or(false)
                    {
                        "#".into()
                    } else {
                        " ".into()
                    }
                })
                .collect::<Line>()
        });

        let text = Text::from_iter(lines);
        Paragraph::new(text).scroll((0, 0)).render(area, buf);
    }
}
