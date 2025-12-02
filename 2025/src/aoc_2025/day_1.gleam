import gleam/int
import gleam/list
import gleam/string

pub fn pt_1(input: String) {
  let rotations = parse(input)
  list.fold(rotations, Acc(dial: 50, zeroes: 0), fn(acc, rotation) {
    let dial =
      case rotation {
        Left(n) -> acc.dial - n
        Right(n) -> acc.dial + n
      }
      % 100
    // Count if we end on the zero
    let zeroes = case dial {
      0 -> acc.zeroes + 1
      _ -> acc.zeroes
    }
    Acc(dial, zeroes)
  }).zeroes
}

pub fn pt_2(input: String) {
  let rotations = parse(input)
  list.fold(rotations, Acc(dial: 50, zeroes: 0), fn(acc, rotation) {
    let quantum_before = quantum(acc.dial)
    let dial = case rotation {
      Left(n) -> acc.dial - n
      Right(n) -> acc.dial + n
    }
    // Count how many times we crossed zero on this
    let zeroes = int.absolute_value(quantum(dial) - quantum_before)
    Acc(dial % 100, acc.zeroes + zeroes)
  }).zeroes
}

fn parse(input: String) -> List(Rotation) {
  input
  |> string.split(on: "\n")
  |> list.map(fn(line) {
    let direction = line |> string.slice(0, 1)
    let assert Ok(n) = line |> string.slice(1, 10) |> int.parse()
    case direction {
      "L" -> Left(n)
      "R" -> Right(n)
      _ -> panic as "invalid input line"
    }
  })
}

/// it's a funny name
/// [-100, 0) => -1
/// [0, 100) => 0
/// [100, 200) => 1
fn quantum(dial: Int) -> Int {
  let assert Ok(q) = int.floor_divide(dial, 100)
  q
}

pub type Acc {
  Acc(dial: Int, zeroes: Int)
}

pub type Rotation {
  Left(n: Int)
  Right(n: Int)
}
