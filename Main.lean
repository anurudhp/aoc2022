import Init.System.IO
import Aoc
import Aoc.Day01 

def run (sol : String → String) (inf : String) : IO Unit := do
  let inp ← IO.FS.readFile inf
  IO.println (sol inp)

def runTask (silent : Bool) (n : Int) : IO Unit :=
  match n with
  | 1 => run Day01.main "inputs/day01.in"
  | _ => do
    if not silent then
      IO.println s!"[!] Invalid Task {n}"

def main (args : List String) : IO Unit := do
  if args.isEmpty then
    IO.println "[!] Running all tasks!"
    for i in [1:26] do
      runTask true i
  else
    runTask false args.head!.toNat!

