import Aoc
import Aoc.Lib.List
import Aoc.Lib.IntList

namespace Day10

inductive Command :=
| Noop
| AddX (v : Int)

def parseCommand (s : String) : Command :=
  if s.startsWith "addx" then
    Command.AddX (s.drop 5 |>.toInt!)
  else
    Command.Noop

def Hist := List Int

def init : Hist := [1, 1]

def Command.run (xs : Hist) : Command →  Hist
| Noop   => x :: xs
| AddX v => (x + v) :: x :: xs
  where
    x := xs.head!

def main (inp : String) : String :=
  let hist := lines inp 
   |>.map parseCommand
   |>.foldl Command.run init
   |>.reverse
  
  let part1 := [20, 60, 100, 140, 180, 220].map (λ i => i * hist.get! i) |> sum

  s!"{part1}"