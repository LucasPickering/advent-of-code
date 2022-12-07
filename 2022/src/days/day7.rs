use crate::days::Solver;
use lazy_static::lazy_static;
use regex::Regex;
use std::{fmt::Display, str::Lines};

const DISK_SIZE: usize = 70000000;
const UPDATE_SIZE: usize = 30000000;

pub struct Day7Solver;

impl Solver for Day7Solver {
    fn part1(&self, input: String) -> String {
        let root_dir = Directory::parse_root(&input);
        println!("{}", root_dir);
        // Find all fs objects with size 100000 *or less*. For simplicity, lets
        // assume the root is not under that threshold
        root_dir
            .children_iter()
            .filter_map(|(fs_object, _)| fs_object.directory())
            .map(Directory::size)
            .filter(|size| *size <= 100000)
            .sum::<usize>()
            .to_string()
    }

    fn part2(&self, input: String) -> String {
        let root_dir = Directory::parse_root(&input);
        println!("{}", root_dir);

        // Figure out how much room we need to make
        let current_free_space = DISK_SIZE - root_dir.size();
        let space_to_free = UPDATE_SIZE - current_free_space;

        // Find the smallest dir that we can delete to make the space needed
        root_dir
            .children_iter()
            .filter_map(|(fs_object, _)| fs_object.directory())
            .map(Directory::size)
            .filter(|size| *size >= space_to_free)
            .min()
            .unwrap()
            .to_string()
    }
}

#[derive(Clone, Debug)]
enum FsObject {
    File(File),
    Directory(Directory),
}
#[derive(Clone, Debug)]
struct File {
    name: String,
    size: usize,
}

#[derive(Clone, Debug)]
struct Directory {
    name: String,
    children: Vec<FsObject>,
}

impl Directory {
    /// Parse input and return the root dir
    fn parse_root(input: &str) -> Self {
        let mut root_dir = Directory {
            name: "/".into(),
            children: Vec::new(),
        };
        let mut lines = input.lines();
        // We expect the first line to just be `cd /`, so skip that
        assert_eq!(lines.next(), Some("$ cd /"));
        // Parse recursively
        root_dir.parse_children(&mut lines);
        root_dir
    }

    fn parse_children(&mut self, lines: &mut Lines) {
        lazy_static! {
            static ref CD_REGEX: Regex = Regex::new(r"^\$ cd (.+)$").unwrap();
            static ref DIRECTORY_REGEX: Regex =
                Regex::new(r"^dir (.+)$").unwrap();
            static ref FILE_REGEX: Regex = Regex::new(r"^(\d+) (.+)$").unwrap();
        }

        // We can't use a for loop because we have to pass `lines` down
        // recursively, which means we have to be able to give up the reference
        // to lines between iterations of this loop.
        while let Some(line) = lines.next() {
            // ls gets ignored cause it sucks

            // cd changes current directory (duh?)
            if let Some(caps) = CD_REGEX.captures(line) {
                let dir_name = caps.get(1).unwrap().as_str();
                if dir_name == ".." {
                    // cd .. means we're done with this dir
                    return;
                } else {
                    // We must go deeper
                    let next_dir = self
                        .children
                        .iter_mut()
                        .find_map(|child| match child {
                            FsObject::Directory(directory) => {
                                if directory.name == dir_name {
                                    Some(directory)
                                } else {
                                    None
                                }
                            }
                            FsObject::File(_) => None,
                        })
                        .expect("No child found");
                    next_dir.parse_children(lines);
                }
            }

            // Add a directory to children
            if let Some(caps) = DIRECTORY_REGEX.captures(line) {
                let name = caps.get(1).unwrap().as_str();
                self.children.push(FsObject::Directory(Directory {
                    name: name.into(),
                    children: Vec::new(),
                }));
            }

            // Add a file to children
            if let Some(caps) = FILE_REGEX.captures(line) {
                let name = caps.get(2).unwrap().as_str();
                let size = caps.get(1).unwrap().as_str().parse().unwrap();

                self.children.push(FsObject::File(File {
                    name: name.into(),
                    size,
                }));
            }
        }
    }

    /// Recursively calculate this directory's size
    fn size(&self) -> usize {
        self.children.iter().map(FsObject::size).sum()
    }

    /// Iterate over all children in this directory, *recursively*
    fn children_iter(&self) -> impl Iterator<Item = (&FsObject, usize)> {
        DirectoryIter::new(self)
    }
}

impl FsObject {
    fn size(&self) -> usize {
        match self {
            Self::File(File { size, .. }) => *size,
            Self::Directory(directory) => directory.size(),
        }
    }

    /// If this is a directory, return it. If it's a file, return `None`.
    fn directory(&self) -> Option<&Directory> {
        match self {
            FsObject::Directory(directory) => Some(directory),
            FsObject::File(_) => None,
        }
    }
}

/// Iterate over a directory's children, recursively, but **not the directory
/// itself!**. This is a mild hack, because we'd need to wrap self in FsObject
/// which means we'd have to own it, and that's a whole mess.
struct DirectoryIter<'a> {
    current: &'a Directory,
    next_child: usize,
    /// A history of parent directories, each one paired with the number of the
    /// child that we were going to iterate over next
    stack: Vec<(&'a Directory, usize)>,
}

impl<'a> DirectoryIter<'a> {
    fn new(directory: &'a Directory) -> Self {
        Self {
            current: directory,
            next_child: 0,
            stack: Vec::new(),
        }
    }
}

impl<'a> Iterator for DirectoryIter<'a> {
    type Item = (&'a FsObject, usize);

    fn next(&mut self) -> Option<Self::Item> {
        match self.current.children.get(self.next_child) {
            Some(child) => {
                // Depth is 0 at root, one for its children, etc. We need to
                // grab this *before* modifying the stack below
                let depth = self.stack.len() + 1;

                // Update state according to the child type
                match child {
                    // Child is a file - advance to the next, then emit
                    FsObject::File(_) => {
                        self.next_child += 1;
                    }
                    // Child is a directory - emit it, but set us up to go there
                    // next
                    FsObject::Directory(directory) => {
                        // Move onto the child, but store our history for when
                        // we need to bounce back up
                        self.stack.push((self.current, self.next_child + 1));
                        self.current = directory;
                        self.next_child = 0;
                    }
                }

                // Either way, emit this child
                Some((child, depth))
            }
            // No children left here, jump back to the parent
            None => match self.stack.pop() {
                Some((parent, next_child)) => {
                    self.current = parent;
                    self.next_child = next_child;
                    // Try again now that we're higher up the stack
                    self.next()
                }
                // We have no children left, and there's no parent to go to,
                // so we're done
                None => None,
            },
        }
    }
}

impl Display for Directory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "- {} (dir)", self.name)?;
        // We can do this in a flat loop since the iterator already handles
        // flattening
        for (fs_object, depth) in self.children_iter() {
            // dbg!(fs_object.name(), depth);
            // Add indentation
            for _ in 0..depth {
                write!(f, "  ")?;
            }
            write!(f, "- ")?;
            match fs_object {
                FsObject::File(file) => writeln!(f, "{}", file)?,
                FsObject::Directory(directory) => {
                    writeln!(f, "{} (dir)", directory.name)?
                }
            }
        }
        Ok(())
    }
}

impl Display for File {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} (file, size={})", self.name, self.size)
    }
}
