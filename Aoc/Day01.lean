import Aoc
import Aoc.Lib.List
import Aoc.Lib.Mergesort

def part1 (xs : List Int) : Int := xs.foldl max 0 

def part2 (xs : List Int) : Int := xs.mergeSort |>.reverse |>.take 3 |>.foldl .add 0 

def main : IO Unit := IO.interact $ λ input =>
  let input :=
    lines input
      |>.splitOn String.isEmpty
      |>.map (.map String.toInt!)
      |>.map (List.foldl .add 0)
  s!"{part1 input}, {part2 input}"
