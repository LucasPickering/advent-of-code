use regex::Regex;
use std::sync::LazyLock;

pub struct Solver {
    instructions: Vec<Instruction>,
}

impl super::Solver for Solver {
    fn new(input: String) -> Self {
        static REGEX: LazyLock<Regex> = LazyLock::new(|| {
            Regex::new(r#"(mul\((\d+),(\d+)\))|(do\(\))|(don't\(\))"#).unwrap()
        });
        let instructions = REGEX
            .captures_iter(&input)
            .map(|cap| match &cap[0] {
                "do()" => Instruction::Do,
                "don't()" => Instruction::Dont,
                _ => Instruction::Mul(
                    cap[2].parse::<i32>().unwrap(),
                    cap[3].parse::<i32>().unwrap(),
                ),
            })
            .collect();
        Self { instructions }
    }

    fn part1(self: Box<Self>) -> String {
        let sum: i32 = self
            .instructions
            .into_iter()
            .filter_map(|instruction| match instruction {
                Instruction::Mul(a, b) => Some(a * b),
                Instruction::Do | Instruction::Dont => None,
            })
            .sum();
        sum.to_string()
    }

    fn part2(self: Box<Self>) -> String {
        let (_, sum) = self.instructions.into_iter().fold(
            (true, 0),
            |(enabled, sum), instruction| match instruction {
                Instruction::Mul(a, b) if enabled => (true, sum + (a * b)),
                Instruction::Mul(_, _) => (false, sum),
                Instruction::Do => (true, sum),
                Instruction::Dont => (false, sum),
            },
        );
        sum.to_string()
    }
}

fn parse_input_p1(input: &str) -> impl '_ + Iterator<Item = (i32, i32)> {
    static REGEX: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r#"mul\((\d+),(\d+)\)"#).unwrap());
    REGEX.captures_iter(input).map(|cap| {
        (
            cap[1].parse::<i32>().unwrap(),
            cap[2].parse::<i32>().unwrap(),
        )
    })
}

enum Instruction {
    Mul(i32, i32),
    Do,
    Dont,
}
