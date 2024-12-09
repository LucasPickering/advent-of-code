use crate::tui::Tui;
use std::{
    collections::{HashMap, VecDeque},
    fmt::{self, Display},
    iter,
};

#[derive(Debug)]
pub struct Solver {
    file_system: Vec<Block>,
    /// Points into the file system of the free spaces, in order left-to-right.
    /// Makes it easy to find the next available free space
    free_spaces: VecDeque<FreeSpace>,
}

#[derive(Copy, Clone, Debug)]
enum Block {
    File(File),
    Free,
}

type FileId = usize;

#[derive(Copy, Clone, Debug)]
struct File {
    id: FileId,
    /// Store the size of the entire file, so we can quickly jump between
    /// beginning and end
    size: usize,
}

#[derive(Copy, Clone, Debug)]
struct FreeSpace {
    /// Index of the first block of the free space
    start: usize,
    /// Number of contiguous blocks in the free space
    size: usize,
}

impl super::Solver for Solver {
    fn new(input: String, _: Tui) -> Self {
        let mut file_system = Vec::new();
        let mut free_spaces = VecDeque::with_capacity(input.len() / 2);
        for (i, c) in input.trim().chars().enumerate() {
            let size = c.to_digit(10).expect("invalid char") as usize;
            // Alternate between files and free space
            let iter = if i % 2 == 0 {
                // The even blocks are files, and IDs are incremental
                // within files
                iter::repeat(Block::File(File { id: i / 2, size }))
            } else if size > 0 {
                free_spaces.push_back(FreeSpace {
                    start: file_system.len(),
                    size,
                });
                iter::repeat(Block::Free)
            } else {
                // Empty free space, this will become an empty iterator
                iter::repeat(Block::Free)
            };
            file_system.extend(iter.take(size));
        }
        Self {
            file_system,
            free_spaces,
        }
    }

    fn part1(mut self: Box<Self>) -> String {
        self.condense_fs_p1();
        self.checksum().to_string()
    }

    fn part2(mut self: Box<Self>) -> String {
        self.condense_fs_p2();
        self.checksum().to_string()
    }
}

impl Solver {
    /// Starting from the end, move each file block into the leftmost free space
    fn condense_fs_p1(&mut self) {
        loop {
            self.trim_end();
            // If there's free space left, then we have work to do still
            let Some(free_space) =
                self.pop_first_free_space(self.file_system.len(), 0)
            else {
                break;
            };
            // Grab the final file
            let file = self.pop_last_file();

            #[allow(clippy::comparison_chain)] // stupid rule
            if file.size == free_space.size {
                // This file fits perfectly in the free space
                self.file_system[free_space.start..free_space.end()]
                    .fill(Block::File(file));
            } else if file.size < free_space.size {
                // File is smaller than the space. Fill the beginning and add
                // the space back to the pool
                let end = free_space.start + file.size;
                self.file_system[free_space.start..end].fill(Block::File(file));
                self.free_spaces.push_front(FreeSpace {
                    start: end,
                    size: free_space.size - file.size,
                });
            } else {
                // File is too big for the space. Migrate the end and push the
                // beginning back to the end of the fs
                let file_moved = File {
                    id: file.id,
                    size: free_space.size,
                };
                let file_remain = File {
                    id: file.id,
                    size: file.size - file_moved.size,
                };
                self.file_system[free_space.start..free_space.end()]
                    .fill(Block::File(file_moved));
                self.file_system.extend(
                    iter::repeat(Block::File(file_remain))
                        .take(file_remain.size),
                );
            }
        }
        assert!(
            self.file_system
                .iter()
                .all(|series| matches!(series, Block::File { .. })),
            "Free space should be eliminated"
        );
    }

    /// Starting from the end, move each file into the leftmost free space
    /// without breaking files apart
    fn condense_fs_p2(&mut self) {
        // Get a point to each file in the file system
        let mut file_map: HashMap<FileId, usize> = self
            .file_system
            .iter()
            .enumerate()
            // Iterate right->left so the first block of each file takes
            // precedence, i.e. we get the start index of each file
            .rev()
            .filter_map(|(i, block)| {
                if let Block::File(file) = block {
                    Some((file.id, i))
                } else {
                    None
                }
            })
            .collect();
        let last_file_id = *file_map.keys().max().expect("empty file map");

        // Attempt to move each file only once
        for file_id in (0..=last_file_id).rev() {
            let file_index = file_map[&file_id];
            let Block::File(file) = self.file_system[file_index] else {
                panic!("File is not a file")
            };
            let Some(free_space) = self.find_free_space(file_index, file.size)
            else {
                // Nowhere to move this file
                continue;
            };
            // Move the file to the free space
            let start = free_space.start;
            let end = start + file.size;
            self.file_system[start..end].fill(Block::File(file));
            file_map.insert(file.id, start);
            // Free up the old space
            self.file_system[file_index..file_index + file.size]
                .fill(Block::Free);
        }
    }

    fn checksum(&self) -> usize {
        self.file_system
            .iter()
            .enumerate()
            .map(|(i, block)| match block {
                Block::File(File { id, .. }) => i * id,
                Block::Free => 0,
            })
            .sum()
    }

    /// Trim free space off the end of the file system
    fn trim_end(&mut self) {
        // Theoretically there should never be contiguous free space because
        // they should be joined together, but just to make sure we'll loop
        // multiple times
        while let Some(free_space) = self.free_spaces.back() {
            if free_space.end() == self.file_system.len() {
                self.drop_last(free_space.size);
                self.free_spaces.pop_back();
            } else {
                break;
            }
        }
        assert!(
            matches!(self.file_system.last(), Some(Block::File(_))),
            "FS does not end in a file after trimming"
        );
    }

    /// Find the leftmost free space that's at least the given minimum size,
    /// but don't go right of the given end index
    fn find_free_space(
        &self,
        end: usize,
        min_size: usize,
    ) -> Option<FreeSpace> {
        let mut start = None;
        for i in 0..end {
            if let Block::Free = self.file_system[i] {
                let start = if let Some(start) = start {
                    start
                } else {
                    start = Some(i);
                    i
                };
                let size = i - start + 1;
                if size >= min_size {
                    return Some(FreeSpace { start, size });
                }
            } else {
                start = None;
            }
        }
        None
    }

    /// Pop the first free space left of the given end index and  at least as
    /// large as the given size off the free space queue
    fn pop_first_free_space(
        &mut self,
        end: usize,
        min_size: usize,
    ) -> Option<FreeSpace> {
        let index = self
            .free_spaces
            .iter()
            .take(end)
            .position(|free_space| free_space.size >= min_size)?;
        self.free_spaces.remove(index)
    }

    /// Pop all blocks of the last file off the end of the fs. This assumes
    /// the fs has been trimmed so that there's no free space at the end
    fn pop_last_file(&mut self) -> File {
        let Some(Block::File(file)) = self.file_system.last() else {
            unreachable!("FS should be trimmed before getting last file");
        };
        let file = *file;
        self.drop_last(file.size);
        file
    }

    /// Drop the final n blocks of the file system
    fn drop_last(&mut self, size: usize) {
        self.file_system.truncate(self.file_system.len() - size);
    }
}

impl FreeSpace {
    fn end(&self) -> usize {
        self.start + self.size
    }
}

impl Display for Solver {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for block in &self.file_system {
            match block {
                Block::File(file) => write!(f, "{}", file.id)?,
                Block::Free => write!(f, ".")?,
            }
        }
        Ok(())
    }
}
