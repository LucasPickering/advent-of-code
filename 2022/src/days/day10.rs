use crate::{days::Solver, util::words};
use itertools::Itertools;
use log::info;
use std::{collections::HashMap, fmt::Display, ops::RangeInclusive};

const SCREEN_WIDTH: usize = 40;
const CHECKPOINTS: &[usize] = &[20, 60, 100, 140, 180, 220];

pub struct Day10Solver;

impl Solver for Day10Solver {
    fn part1(&self, input: String) -> String {
        let instructions = parse_input(&input);
        let mut state_machine = StateMachine::default();

        for instruction in instructions {
            state_machine.run_instruction(instruction);
        }

        info!("{}", state_machine);
        state_machine
            .checkpoints
            .values()
            .map(|val| val.unwrap())
            .sum::<isize>()
            .to_string()
    }

    fn part2(&self, input: String) -> String {
        let instructions = parse_input(&input);
        let mut state_machine = StateMachine::default();

        for instruction in instructions {
            state_machine.run_instruction(instruction);
        }

        state_machine.screen
    }
}

#[derive(Clone, Debug)]
struct StateMachine {
    cycle: usize,
    x: isize,
    checkpoints: HashMap<usize, Option<isize>>,
    screen: String,
}

#[derive(Copy, Clone, Debug)]
enum Instruction {
    Noop,
    Addx(isize),
}

fn parse_input(input: &str) -> Vec<Instruction> {
    input
        .lines()
        .map(|line| match words(line).as_slice() {
            &["noop"] => Instruction::Noop,
            &["addx", val] => Instruction::Addx(val.parse().unwrap()),
            other => panic!("Unrecognized instruction: {other:?}"),
        })
        .collect()
}

impl StateMachine {
    fn run_instruction(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::Noop => {
                self.add_cycle();
            }
            Instruction::Addx(val) => {
                // The change doesn't register until after the second cycle
                self.add_cycle();
                self.add_cycle();
                self.x += val;
            }
        }
    }

    fn signal_strength(&self) -> isize {
        (self.cycle as isize) * self.x
    }

    fn sprite_position(&self) -> RangeInclusive<isize> {
        (self.x - 1)..=(self.x + 1)
    }

    fn add_cycle(&mut self) {
        // Write to CRT
        let screen_x = self.cycle % SCREEN_WIDTH;
        let c = if self.sprite_position().contains(&(screen_x as isize)) {
            '#'
        } else {
            '.'
        };
        self.screen.push(c);
        if screen_x == SCREEN_WIDTH - 1 {
            self.screen.push('\n');
        }

        self.cycle += 1;

        // If this is a cycle we care about, store it as a checkpoint
        let signal_strength = self.signal_strength();
        self.checkpoints.entry(self.cycle).and_modify(|val| {
            *val = Some(signal_strength);
        });
    }
}

impl Default for StateMachine {
    fn default() -> Self {
        Self {
            cycle: 0,
            x: 1,
            checkpoints: CHECKPOINTS
                .iter()
                .copied()
                .map(|checkpoint| (checkpoint, None))
                .collect(),
            screen: String::new(),
        }
    }
}

impl Display for StateMachine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "cycle={}; X={}", self.cycle, self.x)?;
        for (checkpoint, signal_strength) in self.checkpoints.iter().sorted() {
            writeln!(
                f,
                "{} - {}",
                checkpoint,
                signal_strength
                    .map(|val| val.to_string())
                    .unwrap_or_else(|| "_".into()),
            )?;
        }
        Ok(())
    }
}
