use crate::{
    tui::Tui,
    util::{Cluster, Direction, Grid, Point2, Vector2},
};

pub struct Solver {
    grid: Grid<Plant>,
}

type Region<'a> = Cluster<'a, Plant, Plant>;

type Plant = char;

impl super::Solver for Solver {
    fn new(input: String, _: Tui) -> Self {
        let grid = Grid::from_rows(input.lines().map(|line| line.chars()));
        Self { grid }
    }

    fn part1(self: Box<Self>) -> String {
        let regions = self.grid.clusters(|plant| *plant);
        regions
            .into_iter()
            .map(|region| self.cost(&region))
            .sum::<usize>()
            .to_string()
    }

    fn part2(self: Box<Self>) -> String {
        let regions = self.grid.clusters(|plant| *plant);
        regions
            .into_iter()
            .map(|region| self.discounted_cost(&region))
            .sum::<usize>()
            .to_string()
    }
}

impl Solver {
    fn cost(&self, region: &Region) -> usize {
        self.area(region) * self.perimeter(region)
    }

    fn perimeter(&self, region: &Region) -> usize {
        // For each plot, its contribution to the perimeter is 4 minus the
        // number of adjacent plots in the region
        region
            .cells
            .iter()
            .map(|(point, _)| {
                let adjacent_same = self
                    .grid
                    .adjacents(*point)
                    .filter(|(_, adj_plant)| **adj_plant == region.class)
                    .count();
                // We need a fence if the adjacent is a different plant OR the
                // grid boundary
                4 - adjacent_same
            })
            .sum()
    }

    fn area(&self, region: &Region) -> usize {
        region.cells.len()
    }

    fn discounted_cost(&self, region: &Region) -> usize {
        self.area(region) * self.num_sides(region)
    }

    fn num_sides(&self, region: &Region) -> usize {
        let is_in_region = |point: Point2, vector: Vector2| -> bool {
            if let Some(adj) = point + vector {
                self.grid.get(adj) == Some(&region.class)
            } else {
                false
            }
        };

        // Is there a corner on the clockwise side of this direction?
        let is_corner = |point: Point2, direction: Direction| -> bool {
            let dir1 = direction;
            let dir2 = dir1.clockwise();
            let is_boundary1 = !is_in_region(point, dir1.vector());
            let is_boundary2 = !is_in_region(point, dir2.vector());
            // If there's a boundary in both directions, it's an outside
            // corner
            let is_outside_corner = is_boundary1 && is_boundary2;
            // If neither is a boundary, but the sum is outside the cluster,
            // we have an inside corner
            let is_inside_corner = !is_boundary1
                && !is_boundary2
                && !is_in_region(point, dir1.vector() + dir2.vector());
            is_outside_corner || is_inside_corner
        };

        let count_corners = |point: Point2| -> usize {
            Direction::ALL
                .into_iter()
                .filter(move |direction| is_corner(point, *direction))
                .count()
        };

        // Number of sides is equal to the number of corners. Count the number
        // of corners on each cell in the region and just sum them
        region
            .cells
            .iter()
            .map(|(point, _)| count_corners(*point))
            .sum()
    }
}
