import Aoc
import Aoc.Day01
import Aoc.Day02
import Aoc.Day03
import Aoc.Day04
import Aoc.Day05
import Aoc.Day06
import Aoc.Day07
import Aoc.Day08
import Aoc.Day09
import Aoc.Day10
import Aoc.Day11

partial def readInput : IO String := do
  let stdin ← IO.getStdin
  let line ← stdin.getLine
  if line.length == 0 then
    return ""
  else
    let rest ← readInput
    return line ++ rest 

def run (interactive? : Bool) (sol : String → String) (n : String)  : IO Unit := do
  let inp ← if interactive? then readInput else IO.FS.readFile s!"inputs/day{n}.in"
  IO.println s!"day{n}: {sol inp}"

def runTask (silent? : Bool) (interactive? : Bool) (n : Int) : IO Unit :=
  let runner := run interactive?
  match n with
  | 1  => runner Day01.main "01"
  | 2  => runner Day02.main "02"
  | 3  => runner Day03.main "03"
  | 4  => runner Day04.main "04"
  | 5  => runner Day05.main "05"
  | 6  => runner Day06.main "06"
  | 7  => runner Day07.main "07"
  | 8  => runner Day08.main "08"
  | 9  => runner Day09.main "09"
  | 10 => runner Day10.main "10"
  | 11 => runner Day11.main "11"
  | _  => do
    if not silent? then
      IO.println s!"[!] Invalid Task {n}"

def main (args : List String) : IO Unit := do
  if args.isEmpty then
    IO.println "[!] Running all tasks!"
    for i in [1:26] do
      runTask true false i
  else if args.length == 1 then
    runTask false false args.head!.toNat!
  else if args.length == 2 ∧ args.get! 1 == "-i" then
    runTask false true args.head!.toNat!
  else
    IO.println "[!] invalid command line arguments"

