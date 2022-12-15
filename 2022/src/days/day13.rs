use itertools::Itertools;
use log::{debug, trace};
use nom::{
    branch::alt,
    character::complete::{char, digit1, multispace0},
    combinator::{all_consuming, map, map_res},
    multi::{many0, separated_list0},
    sequence::delimited,
    IResult,
};
use std::{fmt::Display, iter};

pub struct Solver;

impl super::Solver for Solver {
    fn part1(&self, input: String) -> String {
        let expressions = parse_input(&input);
        // Pair the expressions up
        let pairs: Vec<(Expression, Expression)> =
            expressions.into_iter().tuples().collect();

        for (expression1, expression2) in &pairs {
            debug!("({expression1}, {expression2})");
        }

        // Get the sum of the sorted pair indices
        let index_sum: usize = pairs
            .into_iter()
            .enumerate()
            .filter(|(_, (e1, e2))| e1 <= e2)
            // Problem indices start at 1
            .map(|(i, _)| i + 1)
            .sum();
        index_sum.to_string()
    }

    fn part2(&self, input: String) -> String {
        let delimiters = [
            Expression::List(vec![Expression::List(vec![Expression::Int(2)])]),
            Expression::List(vec![Expression::List(vec![Expression::Int(6)])]),
        ];
        let mut expressions = parse_input(&input);
        expressions.extend(delimiters.clone());
        expressions.sort();
        // Find the index of the two delimiter packets, and multiply them
        delimiters
            .into_iter()
            .map(|delimiter| {
                expressions.iter().position(|e| e == &delimiter).unwrap() + 1
            })
            .product::<usize>()
            .to_string()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum Expression {
    Int(usize),
    List(Vec<Expression>),
}

fn parse_input(input: &str) -> Vec<Expression> {
    /// Parse a single expression and return the remaining data to be parsed
    fn parse_expression(input: &str) -> IResult<&str, Expression> {
        trace!("Parsing expression: {input}");
        delimited(
            multispace0,
            alt((
                map(parse_list, Expression::List),
                map(parse_int, Expression::Int),
            )),
            multispace0,
        )(input)
    }

    fn parse_list(input: &str) -> IResult<&str, Vec<Expression>> {
        trace!("Parsing list: {input}");
        delimited(
            char('['),
            separated_list0(char(','), parse_expression),
            char(']'),
        )(input)
    }

    fn parse_int(input: &str) -> IResult<&str, usize> {
        trace!("Parsing int: {input}");
        map_res(digit1, str::parse)(input)
    }

    // Parse everything
    let (_, expressions) =
        all_consuming(many0(parse_expression))(input).unwrap();
    expressions
}

impl Ord for Expression {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            // Int vs Int
            (Self::Int(v1), Self::Int(v2)) => v1.cmp(v2),
            // List vs List
            (Self::List(l1), Self::List(l2)) => l1.iter().cmp(l2),
            // Mismatch - turn the solo value into a list and try again
            (Self::Int(v1), Self::List(l2)) => {
                iter::once(&Self::Int(*v1)).cmp(l2)
            }
            (Self::List(l1), Self::Int(v2)) => {
                l1.iter().cmp(iter::once(&Self::Int(*v2)))
            }
        }
    }
}

impl PartialOrd for Expression {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Int(int) => write!(f, "{int}")?,
            Expression::List(values) => {
                write!(f, "[")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{value}")?;
                }
                write!(f, "]")?;
            }
        }
        Ok(())
    }
}
