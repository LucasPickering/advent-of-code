use crate::{
    tui::{GridWidget, Level, Tui},
    util::{Grid, Point2, Vector2},
};

pub struct Solver {
    grid: Grid<Char>,
    tui: Tui,
}

impl super::Solver for Solver {
    fn new(input: String, tui: Tui) -> Self {
        let grid = Grid::from_rows(input.lines().map(|line| {
            line.chars().map(|c| match c {
                'X' => Char::X,
                'M' => Char::M,
                'A' => Char::A,
                'S' => Char::S,
                c => panic!("Unexpected char: {c}"),
            })
        }));
        Self { grid, tui }
    }

    fn part1(mut self: Box<Self>) -> String {
        let num_xmas: usize =
            self.grid.points().map(|point| self.count_xmas(point)).sum();
        num_xmas.to_string()
    }

    fn part2(self: Box<Self>) -> String {
        todo!()
    }
}

impl Solver {
    /// Count how many XMAS combinations start from the given point
    fn count_xmas(&mut self, point: Point2<usize>) -> usize {
        self.tui.show(
            Level::Debug,
            GridWidget {
                grid: &self.grid,
                selected_red: &[point],
                selected_green: &[],
            },
        );
        if self.grid[point] == Char::X {
            Vector2::ONE_STEP
                .into_iter()
                .filter(|vector| self.is_xmas(point, *vector))
                .count()
        } else {
            0
        }
    }

    /// Does an XMAS start from the given point and go in the given direction?
    /// Assumes the start is already an X
    fn is_xmas(&mut self, point: Point2<usize>, vector: Vector2) -> bool {
        let mut selected = vec![point];
        for (distance, expected) in [(1, Char::M), (2, Char::A), (3, Char::S)] {
            let Some(point) = point + vector * distance else {
                return false;
            };
            selected.push(point);
            self.tui.show(
                Level::Trace,
                GridWidget {
                    grid: &self.grid,
                    selected_red: &selected,
                    selected_green: &[],
                },
            );
            if !self.grid.is_valid(point) || self.grid[point] != expected {
                return false;
            }
        }
        self.tui.show(
            Level::Info,
            GridWidget {
                grid: &self.grid,
                selected_red: &[],
                selected_green: &selected,
            },
        );
        true
    }
}

#[derive(Copy, Clone, Debug, derive_more::Display, PartialEq)]
enum Char {
    X,
    M,
    A,
    S,
}
