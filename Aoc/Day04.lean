import Aoc
import Aoc.Lib.List

abbrev Elf := Nat × Nat
abbrev ElfPair := Elf × Elf

def mkElf (s : String) : Elf :=
  s.splitOn "-" |>.map String.toNat! |>.first2!

def mkInput (s : String) : ElfPair :=
  s.splitOn "," |>.map mkElf |>.first2!

-- part1
def Elf.contains : Elf → Elf → Bool
| (la, ra), (lb, rb) => la <= lb && rb <= ra

def fullOverlap : ElfPair → Bool
| (a, b) => a.contains b || b.contains a

-- part2
def overlap : ElfPair → Bool
| ((la, ra), (lb, rb)) => ra >= lb && rb >= la

def main : IO Unit := IO.interact $ λ input =>
  let elves := input |> lines |>.map mkInput
  s!"{elves.count fullOverlap}, {elves.count overlap}"
