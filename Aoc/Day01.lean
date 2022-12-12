import Aoc
import Aoc.Lib.IO
import Aoc.Lib.Mergesort
import Aoc.Lib.IntList

def part1 : List Int → Int := maximum 

def part2 : List Int → Int := .mergeSort :>.reverse :>.take 3 :> sum

def main : IO Unit := IO.interact $ λ input =>
  let input :=
    lines input
      |>.splitOn (λ l => l == "") 
      |>.map (.map String.toInt! :> sum)
  s!"{part1 input}, {part2 input}"
