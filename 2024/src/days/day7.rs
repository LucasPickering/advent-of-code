use crate::{days::Part, tui::Tui, util::count_digits};
use std::fmt::Debug;

pub struct Solver {
    equations: Vec<Equation>,
}

type Term = u64;

#[derive(Debug, derive_more::Display)]
#[display("{test_value}: {terms:?}")]
struct Equation<T: AsRef<[Term]> = Vec<Term>> {
    test_value: Term,
    terms: T,
}

impl super::Solver for Solver {
    fn new(input: String, _: Tui) -> Self {
        let equations = input
            .lines()
            .map(|line| {
                let Some((test_value, terms)) = line.split_once(": ") else {
                    panic!("Invalid line: {line:?}");
                };
                let test_value = test_value.parse::<Term>().unwrap();
                let terms = terms
                    .split(" ")
                    .map(|term| term.parse::<Term>().unwrap())
                    .collect();
                Equation { test_value, terms }
            })
            .collect();
        Self { equations }
    }

    fn part1(self: Box<Self>) -> String {
        self.equations
            .into_iter()
            .filter(|eq| eq.is_solvable())
            .map(|equation| equation.test_value)
            .sum::<Term>()
            .to_string()
    }

    fn part2(self: Box<Self>) -> String {
        self.equations
            .into_iter()
            .filter(|eq| eq.is_solvable())
            .map(|equation| equation.test_value)
            .sum::<Term>()
            .to_string()
    }
}

impl<T: AsRef<[Term]> + Debug> Equation<T> {
    fn new(test_value: Term, terms: &[Term]) -> Equation<&[Term]> {
        Equation { test_value, terms }
    }

    fn is_solvable(&self) -> bool {
        // The operators are left-associative, so start at the end and try to
        // figure out the rightmost operator. If the left end is divisible by
        // the right most number, then the last operator could be mult. If not
        // divisible, or it is divisible but the rest of the equation can't be
        // solved that way, try addition.

        match self.terms.as_ref() {
            // We can't reach the empty case because the inputs are never empty,
            // we drop one term at a time, and a single term is always terminal
            [] => unreachable!("terms cannot be empty"),
            [a] => *a == self.test_value,
            [rest @ .., last] => {
                self.is_solvable_mult(rest, *last)
                    || (Part::get().is2()
                        && self.is_solvable_concat(rest, *last))
                    || self.is_solvable_add(rest, *last)
            }
        }
    }

    fn is_solvable_add(&self, rest: &[Term], last: Term) -> bool {
        self.test_value >= last
            && Self::new(self.test_value - last, rest).is_solvable()
    }

    fn is_solvable_mult(&self, rest: &[Term], last: Term) -> bool {
        self.test_value % last == 0
            && Self::new(self.test_value / last, rest).is_solvable()
    }

    fn is_solvable_concat(&self, rest: &[Term], last: Term) -> bool {
        if let Some(prefix) = remove_suffix(self.test_value, last) {
            Self::new(prefix, rest).is_solvable()
        } else {
            false
        }
    }
}

/// Remove the ending digits of a term. Return `None` if the suffix is not
/// actually a suffix of the term.
/// E.g. `remove_suffix(12345, 45)` -> `Some(123)`
fn remove_suffix(a: Term, suffix: Term) -> Option<Term> {
    if a <= suffix {
        return None;
    }

    // suffix = 345 -> len = 3
    let suffix_len = count_digits(suffix);
    // suffix = 345 -> base = 1000
    let prefix_base = 10u64.pow(suffix_len);
    // a = 12345, suffix = 345
    let prefix_with_zeroes = a - suffix;
    if prefix_with_zeroes % prefix_base == 0 {
        Some(prefix_with_zeroes / prefix_base)
    } else {
        None
    }
}
