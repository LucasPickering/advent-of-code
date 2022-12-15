use crate::util::{CapturesExt, MatchExt};
use anyhow::anyhow;
use derive_more::Display;
use itertools::Itertools;
use lazy_static::lazy_static;
use log::{debug, info, trace};
use regex::Regex;
use std::{iter, str::FromStr};

lazy_static! {
    static ref REGEX: Regex = Regex::new(
        r"Monkey (?P<label>\d+):
  Starting items: (?P<starting_items>[\d, ]+)
  Operation: new = old (?P<operator>[+*]) (?P<rhs>old|\d+)
  Test: divisible by (?P<test_divisor>\d+)
    If true: throw to monkey (?P<true_monkey>\d+)
    If false: throw to monkey (?P<false_monkey>\d+)"
    )
    .unwrap();
}

pub struct Solver;

impl super::Solver for Solver {
    fn part1(&self, input: String) -> String {
        let monkeys = parse_input(&input);
        let mut state = State::new(monkeys, true);
        state.run_rounds(20);
        state.monkey_business().to_string()
    }

    fn part2(&self, input: String) -> String {
        let monkeys = parse_input(&input);
        let mut state = State::new(monkeys, false);
        state.run_rounds(10000);
        state.monkey_business().to_string()
    }
}

#[derive(Clone, Debug)]
struct State {
    monkeys: Vec<Monkey>,
    divide_by_3: bool,
    /// The product of all monkeys' divisors. After handling each item, we'll
    /// **mod** the worry level by this number to keep it from growing too big.
    /// Otherwise the numbers get massive and 10k rounds is intractable.
    magic_number: usize,
}

#[derive(Clone, Debug)]
struct Monkey {
    label: usize,
    starting_items: Vec<usize>,
    operation: Operation,
    test_divisor: usize,
    true_monkey: usize,
    false_monkey: usize,
    /// How many items has this monkey inspected?
    inspections: usize,
}

#[derive(Copy, Clone, Debug, Display)]
enum Operand {
    #[display(fmt = "old")]
    Old,
    #[display(fmt = "{}", "0")]
    Literal(usize),
}

#[derive(Copy, Clone, Debug, Display)]
enum Operator {
    #[display(fmt = "+")]
    Add,
    #[display(fmt = "*")]
    Multiply,
}

#[derive(Copy, Clone, Debug)]
struct Operation {
    operator: Operator,
    rhs: Operand,
}

fn parse_input(input: &str) -> Vec<Monkey> {
    // Match the parsing regex as many times as we can. Each match turns into
    // one monkey
    REGEX
        .captures_iter(input)
        .map(|captures| {
            debug!(
                "Parsing matched monkey: {}",
                // Group #0 is always the full match
                captures.get(0).unwrap().as_str()
            );

            let label = captures.name_unwrap("label").parse_unwrap();
            // This should be a comma-separated list, which requires further
            // parsing
            let starting_items = captures
                .name_unwrap("starting_items")
                .as_str()
                .split(", ")
                // Parse and collect any errors, then unwrap at the end
                .map(str::parse)
                .collect::<Result<_, _>>()
                .unwrap();
            let operator = captures.name_unwrap("operator").parse_unwrap();
            let rhs = captures.name_unwrap("rhs").parse_unwrap();
            let test_divisor =
                captures.name_unwrap("test_divisor").parse_unwrap();
            let true_monkey =
                captures.name_unwrap("true_monkey").parse_unwrap();
            let false_monkey =
                captures.name_unwrap("false_monkey").parse_unwrap();

            Monkey {
                label,
                starting_items,
                operation: Operation { operator, rhs },
                test_divisor,
                true_monkey,
                false_monkey,
                inspections: 0,
            }
        })
        .collect()
}

impl State {
    fn new(monkeys: Vec<Monkey>, divide_by_3: bool) -> Self {
        let magic_number =
            monkeys.iter().map(|monkey| monkey.test_divisor).product();
        Self {
            monkeys,
            divide_by_3,
            magic_number,
        }
    }

    fn run_rounds(&mut self, rounds: usize) {
        for round in 1..=rounds {
            info!("Round {round}");
            for i in 0..self.monkeys.len() {
                self.run_monkey(i);
            }
            for monkey in &self.monkeys {
                debug!(
                    "Monkey {} inspected items {} times.",
                    monkey.label, monkey.inspections
                );
            }
        }
    }

    fn run_monkey(&mut self, index: usize) {
        // This monkey will pass items to its friends. We need to defer that
        // passing until after the loop below, since we maintain a mutable
        // reference to this monkey (and therefore the entire vec) throughout
        // the loop below. We *shouldn't* have to do this, but there's no way
        // for the compiler to know that. Technically there are only two
        // possible targets for this monkey so we don't need queues for all of
        // them, but this seems simpler/more efficient than a sparse hashmap
        let mut midair_queues: Vec<Vec<usize>> = iter::repeat_with(Vec::new)
            .take(self.monkeys.len())
            .collect();

        let monkey = &mut self.monkeys[index];
        trace!("Monkey {}:", monkey.label);

        // Drain items as we iterate through them
        for mut worry_level in monkey.starting_items.drain(..) {
            // Follow the steps outlined in the problem statement, with
            // identical logging
            trace!("  Monkey inspects an item with a worry level of {worry_level}.");

            monkey.inspections += 1;
            monkey.operation.apply(&mut worry_level);
            trace!(
                "    Worry level {} by {} to {worry_level}.",
                match monkey.operation.operator {
                    Operator::Add => "increases",
                    Operator::Multiply => "is multiplied",
                },
                monkey.operation.rhs
            );

            if self.divide_by_3 {
                worry_level /= 3usize;
                trace!(
                    "    Monkey gets bored with item. Worry level is divided by 3 to {worry_level}."
                );
            }

            // Mod by the product of all divisors, which will preserve all
            // arithmetic that we care about while preventing numbers from
            // getting too big
            // GOTTA GO FAST
            worry_level %= self.magic_number;
            trace!("Reduced worry level to {worry_level}.");

            let test = worry_level % monkey.test_divisor == 0;
            trace!(
                "    Current worry level {} divisible by {}.",
                if test { "is" } else { "is not" },
                monkey.test_divisor
            );

            // Ship the item accordingly
            let target = if test {
                monkey.true_monkey
            } else {
                monkey.false_monkey
            };
            trace!("    Item with worry level {worry_level} is thrown to monkey {target}.");
            midair_queues[target].push(worry_level);
        }

        // `monkey` reference gets dropped here

        // Drain the queues
        for (target, queue) in midair_queues.into_iter().enumerate() {
            self.monkeys[target].starting_items.extend(queue);
        }
    }

    /// Calculate the monkey business level, which is the number of inspections
    /// of the two most prolific monkeys, multiplied together
    fn monkey_business(&self) -> usize {
        self.monkeys
            .iter()
            .map(|monkey| monkey.inspections)
            .sorted()
            .rev()
            .take(2)
            .product()
    }
}

impl Operation {
    fn apply(self, old: &mut usize) {
        // Resolve rhs expression
        let rhs = match self.rhs {
            Operand::Old => *old,
            Operand::Literal(value) => value,
        };
        match self.operator {
            Operator::Add => {
                *old += rhs;
            }
            Operator::Multiply => {
                *old *= rhs;
            }
        }
    }
}

impl FromStr for Operator {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Self::Add),
            "*" => Ok(Self::Multiply),
            other => Err(anyhow!("Unknown operator: {other}")),
        }
    }
}

impl FromStr for Operand {
    type Err = <usize as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "old" => Ok(Self::Old),
            other => Ok(Self::Literal(other.parse()?)),
        }
    }
}
