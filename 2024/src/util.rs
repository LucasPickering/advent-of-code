use std::ops::{Add, Index, IndexMut, Sub};

#[derive(Copy, Clone, Debug, derive_more::Display, Eq, Hash, PartialEq)]
#[display("({x},{y})")]
pub struct Point2<T = usize> {
    pub x: T,
    pub y: T,
}

impl Point2 {
    /// Get the offset between the two points, i.e. `other - self`
    pub fn displacement(self, other: Point2) -> Vector2 {
        let p1: Point2<i64> = self.into();
        let p2: Point2<i64> = other.into();
        Vector2 {
            x: p2.x - p1.x,
            y: p2.y - p1.y,
        }
    }
}

/// If the addition results in a wrap, return None instead
impl Add<Vector2> for Point2 {
    type Output = Option<Self>;

    fn add(self, rhs: Vector2) -> Self::Output {
        let p: Point2<i64> = self.into();
        Some(Self {
            x: (p.x + rhs.x).try_into().ok()?,
            y: (p.y + rhs.y).try_into().ok()?,
        })
    }
}

impl Sub<Vector2> for Point2 {
    type Output = Option<Point2>;

    fn sub(self, rhs: Vector2) -> Self::Output {
        let p: Point2<i64> = self.into();
        Some(Self {
            x: (p.x - rhs.x).try_into().ok()?,
            y: (p.y - rhs.y).try_into().ok()?,
        })
    }
}

impl From<Point2<usize>> for Point2<i64> {
    fn from(value: Point2<usize>) -> Self {
        Point2 {
            // Assume no input is large enough to overflow
            x: value.x.try_into().unwrap(),
            y: value.y.try_into().unwrap(),
        }
    }
}

#[derive(
    Copy,
    Clone,
    Debug,
    derive_more::Display,
    derive_more::Add,
    derive_more::Sub,
    derive_more::Mul,
)]
#[display("({x},{y})")]
pub struct Vector2 {
    pub x: i64,
    pub y: i64,
}

impl Vector2 {
    /// All vectors in the range (-1,-1) to (1,1), except for (0,0)
    pub const ONE_STEP: [Self; 8] = [
        Self { x: -1, y: -1 },
        Self { x: 0, y: -1 },
        Self { x: 1, y: -1 },
        Self { x: -1, y: 0 },
        Self { x: 1, y: 0 },
        Self { x: -1, y: 1 },
        Self { x: 0, y: 1 },
        Self { x: 1, y: 1 },
    ];
    /// Up/right/left/down
    pub const ADJACENT: [Self; 4] = [
        Self { x: 0, y: -1 },
        Self { x: 1, y: 0 },
        Self { x: 0, y: 1 },
        Self { x: -1, y: 0 },
    ];
}

#[derive(Copy, Clone, Debug, derive_more::Display, Eq, Hash, PartialEq)]
pub enum Direction {
    Up,
    Right,
    Down,
    Left,
}

impl Direction {
    /// Get the next direction clockwise
    pub fn clockwise(&self) -> Direction {
        match self {
            Self::Up => Self::Right,
            Self::Right => Self::Down,
            Self::Down => Self::Left,
            Self::Left => Self::Up,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Grid<T> {
    width: usize,
    height: usize,
    /// Values start at the top-left and move right, then down
    /// Index = y * width + x
    /// Length = width * height
    cells: Vec<T>,
}

impl<T> Grid<T> {
    pub fn from_rows(
        rows: impl Iterator<Item = impl Iterator<Item = T>>,
    ) -> Self {
        let mut rows = rows.enumerate();
        let Some((mut max_y, first_row)) = rows.next() else {
            return Self::default();
        };
        let mut cells: Vec<T> = first_row.collect();
        let width = cells.len();
        for (y, row) in rows {
            max_y = y;
            cells.extend(row);
        }
        let height = max_y + 1;
        Self {
            width,
            height,
            cells,
        }
    }

    /// Iterator over the grid by row
    pub fn rows(
        &self,
    ) -> impl Iterator<Item = impl Iterator<Item = (Point2<usize>, &T)>> {
        (0..self.height).map(move |y| {
            (0..self.width).map(move |x| {
                let point = Point2 { x, y };
                (point, &self[point])
            })
        })
    }

    /// Get an iterator over all the points in this grid, starting at the
    /// top-left and moving right, then down
    pub fn points(&self) -> impl Iterator<Item = Point2<usize>> {
        let width = self.width;
        let height = self.height;
        (0..height).flat_map(move |y| (0..width).map(move |x| Point2 { x, y }))
    }

    /// Get an iterator over all values and points in the grid, starting at the
    /// top-left and moving right, then down
    pub fn iter(&self) -> impl Iterator<Item = (Point2, &T)> {
        self.cells
            .iter()
            .enumerate()
            .map(|(i, cell)| (self.get_point(i), cell))
    }

    pub fn get(&self, point: Point2<usize>) -> Option<&T> {
        if self.is_valid(point) {
            Some(&self[point])
        } else {
            None
        }
    }

    /// Get an iterator of all `(point, value)` pairs in the grid adjacent to
    /// the given point (up/down/left/right)
    pub fn adjacents(
        &self,
        point: Point2<usize>,
    ) -> impl Iterator<Item = (Point2, &T)> {
        Vector2::ADJACENT.into_iter().filter_map(move |vector| {
            let adj_point = (point + vector)?;
            let value = self.get(adj_point)?;
            Some((adj_point, value))
        })
    }

    pub fn is_valid(&self, point: Point2<usize>) -> bool {
        point.x < self.width && point.y < self.height
    }

    fn get_index(&self, point: Point2<usize>) -> usize {
        point.y * self.width + point.x
    }

    fn get_point(&self, index: usize) -> Point2 {
        Point2 {
            x: index % self.width,
            y: index / self.width,
        }
    }
}

impl<T> Default for Grid<T> {
    fn default() -> Self {
        Self {
            width: 0,
            height: 0,
            cells: Vec::new(),
        }
    }
}

impl<T> Index<Point2<usize>> for Grid<T> {
    type Output = T;

    fn index(&self, point: Point2<usize>) -> &Self::Output {
        &self.cells[self.get_index(point)]
    }
}

impl<T> IndexMut<Point2<usize>> for Grid<T> {
    fn index_mut(&mut self, point: Point2<usize>) -> &mut Self::Output {
        let index = self.get_index(point);
        &mut self.cells[index]
    }
}
