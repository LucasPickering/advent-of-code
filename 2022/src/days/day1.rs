use crate::days::Solver;

pub struct Day1Solver;

impl Solver for Day1Solver {
    fn part1(&self, input: String) -> String {
        let elf_calories = calc_elf_calories(input);
        let max = elf_calories.into_iter().max().unwrap();
        max.to_string()
    }

    fn part2(&self, input: String) -> String {
        let mut elf_calories = calc_elf_calories(input);
        elf_calories.sort();
        let max3 = elf_calories[elf_calories.len() - 3..].iter().sum::<u32>();
        max3.to_string()
    }
}

fn calc_elf_calories(input: String) -> Vec<u32> {
    let mut elf_calories = Vec::new();
    let mut current_calories = 0;
    for line in input.lines() {
        if line.is_empty() {
            elf_calories.push(current_calories);
            current_calories = 0;
        } else {
            current_calories += line.parse::<u32>().unwrap();
        }
    }
    elf_calories
}
