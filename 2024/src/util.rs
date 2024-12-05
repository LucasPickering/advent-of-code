use std::ops::{Add, Index, IndexMut};

#[derive(Copy, Clone, Debug, derive_more::Display, Eq, Hash, PartialEq)]
#[display("({x},{y})")]
pub struct Point2<T> {
    pub x: T,
    pub y: T,
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
}

/// If the addition results in a wrap, return None instead
impl Add<Vector2> for Point2<usize> {
    type Output = Option<Self>;

    fn add(self, rhs: Vector2) -> Self::Output {
        let x: i64 = self.x.try_into().unwrap();
        let y: i64 = self.y.try_into().unwrap();
        Some(Self {
            x: (x + rhs.x).try_into().ok()?,
            y: (y + rhs.y).try_into().ok()?,
        })
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

    pub fn get(&self, point: Point2<usize>) -> Option<&T> {
        if self.is_valid(point) {
            Some(&self[point])
        } else {
            None
        }
    }

    pub fn is_valid(&self, point: Point2<usize>) -> bool {
        point.x < self.width && point.y < self.height
    }

    fn get_index(&self, point: Point2<usize>) -> usize {
        point.y * self.width + point.x
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
