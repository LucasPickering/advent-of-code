use crate::tui::Tui;
use std::{cmp::Ordering, collections::HashMap};

pub struct Solver {
    rules: Vec<Rule>,
    updates: Vec<Update>,
}

struct Rule {
    before: Page,
    after: Page,
}

type Update = Vec<Page>;

type Page = u32;

impl super::Solver for Solver {
    fn new(input: String, _: Tui) -> Self {
        let (rules_input, updates_input) = input.split_once("\n\n").unwrap();
        let rules = rules_input
            .lines()
            .map(|line| {
                let (before, after) = line.split_once('|').unwrap();
                Rule {
                    before: before.parse().unwrap(),
                    after: after.parse().unwrap(),
                }
            })
            .collect();
        let updates = updates_input
            .lines()
            .map(|line| {
                line.split(',')
                    .map(|page| page.parse::<Page>().unwrap())
                    .collect()
            })
            .collect();

        Self { rules, updates }
    }

    fn part1(self: Box<Self>) -> String {
        self.updates
            .iter()
            .filter(|update| is_update_valid(&self.rules, update))
            .map(middle)
            .sum::<u32>()
            .to_string()
    }

    fn part2(mut self: Box<Self>) -> String {
        self.updates
            .iter_mut()
            .filter(|update| !is_update_valid(&self.rules, update))
            .map(|update| {
                fix_update(&self.rules, update);
                &*update
            })
            .map(middle)
            .sum::<u32>()
            .to_string()
    }
}

/// Are all pages in accordance with local rules and regulations?
fn is_update_valid(rules: &[Rule], update: &Update) -> bool {
    // For each update, build a mapping of page number : index
    let to_index = update
        .iter()
        .enumerate()
        .map(|(i, page)| (*page, i))
        .collect::<HashMap<_, _>>();
    !rules.iter().any(|rule| rule.is_violated(&to_index))
}

/// Reorder an update to be valid
fn fix_update(rules: &[Rule], update: &mut Update) {
    update.sort_by(|page1, page2| {
        // Find first rule that applies, or default to equal if none do. Since
        // Rust uses a stable sort, the elements won't be moved
        rules
            .iter()
            .find_map(|rule| rule.cmp(*page1, *page2))
            .unwrap_or(Ordering::Equal)
    });
}

fn middle(update: &Update) -> Page {
    update[update.len() / 2]
}

impl Rule {
    fn is_violated(&self, pages: &HashMap<Page, usize>) -> bool {
        matches!(
            (pages.get(&self.before), pages.get(&self.after)),
            // Are both pages in this rule, and if so, does "after" precede "before"?
            (Some(i1), Some(i2)) if i1 > i2
        )
    }

    fn cmp(&self, page1: Page, page2: Page) -> Option<Ordering> {
        if page1 == self.before && page2 == self.after {
            Some(Ordering::Less)
        } else if page1 == self.after && page2 == self.before {
            Some(Ordering::Greater)
        } else {
            None
        }
    }
}
