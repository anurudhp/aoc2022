import Aoc
import Aoc.Lib.List

abbrev Pos3 := Nat × Nat × Nat

def List.sum (l : List Nat) : Nat := l.foldl .add 0

def Pos3.neighbours : Pos3 → List Pos3 
| (x, y, z) => 
  [ (x + 1, y, z)
  , (x - 1, y, z)
  , (x, y + 1, z)
  , (x, y - 1, z)
  , (x, y, z - 1)
  , (x, y, z + 1)
  ]

def List.isPocket (ps : List Pos3) : Pos3 → Bool
| (x, y, z) =>
  [ ps.find? (λ (x', y', z') => x' >= x && y' == y && z' == z)
  , ps.find? (λ (x', y', z') => x' <= x && y' == y && z' == z)
  , ps.find? (λ (x', y', z') => x' == x && y' >= y && z' == z)
  , ps.find? (λ (x', y', z') => x' == x && y' <= y && z' == z)
  , ps.find? (λ (x', y', z') => x' == x && y' == y && z' >= z)
  , ps.find? (λ (x', y', z') => x' == x && y' == y && z' <= z)
  ].all (·.isSome)

def main : IO Unit := IO.interact $ λ input =>
  let pts : List Pos3 := lines input
    |>.map (·.splitOn "," |>.map (·.toNat!) |>.first3!) 

  let area check := 
    let blocked := pts.map (·.neighbours |>.count check) |>.sum
    pts.length * 6 - blocked

  s!"{area pts.elem}, {area pts.isPocket}"