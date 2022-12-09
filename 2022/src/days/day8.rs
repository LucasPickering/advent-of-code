use crate::{days::Solver, util::Direction};
use std::convert;

pub struct Day8Solver;

impl Solver for Day8Solver {
    fn part1(&self, input: String) -> String {
        let grid = TreeGrid::parse(&input);
        let num_visible = grid
            .iter_coords()
            .filter(|((x, y), _)| grid.is_visible(*x, *y))
            .count();
        num_visible.to_string()
    }

    fn part2(&self, input: String) -> String {
        let grid = TreeGrid::parse(&input);
        let max_score = grid
            .iter_coords()
            .map(|((x, y), _)| grid.scenic_score(x, y))
            .max()
            .unwrap();
        max_score.to_string()
    }
}

type Tree = u32;

#[derive(Clone, Debug)]
struct TreeGrid {
    /// A flattened 2D vector. Column-major (same order as the input)
    trees: Vec<Tree>,
    width: usize,
}

impl TreeGrid {
    fn parse(input: &str) -> Self {
        // All lines are the same length, so grab the first to get our width
        let width = input.lines().peekable().peek().unwrap().len();
        let trees = input
            .lines()
            // Parse each char as a separate number
            .flat_map(|line| line.chars().map(|c| c.to_digit(10).unwrap()))
            .collect();
        Self { trees, width }
    }

    /// Get (width, height) of the grid
    fn dimensions(&self) -> (usize, usize) {
        (self.width, self.trees.len() / self.width)
    }

    fn get(&self, x: usize, y: usize) -> Tree {
        self.trees[y * self.width + x]
    }

    /// Get an iterator that projects *out from* a coordinate in one direction
    fn get_ray(
        &self,
        x: usize,
        y: usize,
        direction: Direction,
    ) -> Box<dyn Iterator<Item = Tree> + '_> {
        let (width, height) = self.dimensions();
        match direction {
            Direction::Up => {
                Box::new((0..y).rev().map(move |y| self.get(x, y)))
            }
            Direction::Down => {
                Box::new(((y + 1)..height).map(move |y| self.get(x, y)))
            }
            Direction::Left => {
                Box::new((0..x).rev().map(move |x| self.get(x, y)))
            }
            Direction::Right => {
                Box::new(((x + 1)..width).map(move |x| self.get(x, y)))
            }
        }
    }

    fn is_visible(&self, x: usize, y: usize) -> bool {
        let tree = self.get(x, y);
        Direction::all()
            .into_iter()
            // For each direction, check if all trees in that direction are
            // shorter
            .map(|direction| {
                self.get_ray(x, y, direction).all(|other| other < tree)
            })
            // If the tree is visible in *any* direction, it's visible
            .any(convert::identity)
    }

    fn scenic_score(&self, x: usize, y: usize) -> usize {
        let tree = self.get(x, y);
        Direction::all()
            .into_iter()
            .map(|direction| {
                // It'd be nice to just use .take_while() here, but it doesn't
                // quite work because it excludes the last tree.
                // https://github.com/rust-lang/rust/issues/62208
                let mut num_visible = 0;
                for other in self.get_ray(x, y, direction) {
                    num_visible += 1;
                    if other >= tree {
                        break;
                    }
                }
                num_visible
            })
            .product()
    }

    /// Get an iterator of ((x, y), tree)
    fn iter_coords(&self) -> impl Iterator<Item = ((usize, usize), Tree)> + '_ {
        // Iterate over the vec and convert flat indexes to 2D coords
        self.trees
            .iter()
            .enumerate()
            .map(|(i, tree)| ((i % self.width, i / self.width), *tree))
    }
}
