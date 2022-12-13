import Aoc

def Elf := Nat × Nat deriving Inhabited
def ElfPair := Elf × Elf deriving Inhabited

def first2 [Inhabited α] (l : List α) : α × α := Option.get! $
  match l with
  | [x, y] => some (x, y)
  | _ => none

def mkElf (s : String) : Elf :=
  s.splitOn "-" |>.map String.toNat! |> first2

def mkInput (s : String) : ElfPair :=
  s.splitOn "," |> .map mkElf |> first2

def count (p : ElfPair → Bool) : List ElfPair → Nat :=
  .filter p :> List.length

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
  s!"{count fullOverlap elves}, {count overlap elves}"
