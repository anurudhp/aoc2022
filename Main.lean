import Aoc
import Aoc.Day01
import Aoc.Day02
import Aoc.Day03
import Aoc.Day04
import Aoc.Day05

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
  | 1 => runner Day01.main "01"
  | 2 => runner Day02.main "02"
  | 3 => runner Day03.main "03"
  | 4 => runner Day04.main "04"
  | 5 => runner Day05.main "05"
  | _ => do
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

