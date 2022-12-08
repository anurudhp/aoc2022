import Aoc
import Aoc.Lib.Mergesort
import Aoc.Lib.IntList

namespace Day01 

def part1 : List Int → Int := maximum 

def part2 : List Int → Int := .mergeSort (λ x y => x < y) :>.reverse :>.take 3 :> sum

def main (input : String) : String := 
  let input :=
    lines input
      |>.splitOn (λ l => l == "") 
      |>.map (.map String.toInt! :> sum)
  s!"{part1 input}, {part2 input}"
