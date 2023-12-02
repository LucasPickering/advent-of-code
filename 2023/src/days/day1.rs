use regex::Regex;
use std::sync::LazyLock;

pub struct Solver;

static DIGITS_RAW: &str =
    "(1|2|3|4|5|6|7|8|9|one|two|three|four|five|six|seven|eight|nine)";
static DIGIT_PATTERNS: &[&str] = &[
    "1", "2", "3", "4", "5", "6", "7", "8", "9", "one", "two", "three", "four",
    "five", "six", "seven", "eight", "nine",
];
static DIGIT: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(DIGITS_RAW).unwrap());
static DIGIT_REV: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(&DIGITS_RAW.chars().rev().collect::<String>()).unwrap()
});

impl super::Solver for Solver {
    fn part1(&self, input: String) -> String {
        input
            .lines()
            .map(|line| {
                let first = line.chars().find(char::is_ascii_digit).unwrap();
                let last =
                    line.chars().rev().find(char::is_ascii_digit).unwrap();
                format!("{first}{last}").parse::<u32>().unwrap()
            })
            .sum::<u32>()
            .to_string()
    }

    fn part2(&self, input: String) -> String {
        input
            .lines()
            .map(|line| {
                let matches: Vec<&str> =
                    DIGIT.find_iter(line).map(|m| m.as_str()).collect();
                let first = parse_digit(matches.first().unwrap());
                let last = parse_digit(matches.last().unwrap());
            })
            .sum::<u32>()
            .to_string()
    }
}

fn parse_digit(s: &str) -> u32 {
    match s {
        "1" | "one" => 1,
        "2" | "two" => 2,
        "3" | "three" => 3,
        "4" | "four" => 4,
        "5" | "five" => 5,
        "6" | "six" => 6,
        "7" | "seven" => 7,
        "8" | "eight" => 8,
        "9" | "nine" => 9,
        _ => unreachable!("Unexpected digit string! {s}"),
    }
}
