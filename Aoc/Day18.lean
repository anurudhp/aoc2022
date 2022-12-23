import Aoc
import Aoc.Lib.List

abbrev Pos3 := Nat × Nat × Nat

def Pos3.neighbours : Pos3 → List Pos3 
| (x, y, z) => 
  [ (x + 1, y, z)
  , (x - 1, y, z)
  , (x, y + 1, z)
  , (x, y - 1, z)
  , (x, y, z - 1)
  , (x, y, z + 1)
  ]

def main : IO Unit := IO.interact $ λ input =>
  let pts : List Pos3 := lines input
    |>.map (·.splitOn "," |>.map (·.toNat!) |>.first3!) 

  let air := pts.map (·.neighbours |>.filter (not ∘ pts.elem)) |>.join

  s!"{air.length}"