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

def Pos3.visible (ps : List Pos3) : Pos3 → Bool
| (x, y, z) =>
  [ ps.find? (λ (x', y', z') => x' >= x && y' == y && z' == z)
  , ps.find? (λ (x', y', z') => x' <= x && y' == y && z' == z)
  , ps.find? (λ (x', y', z') => x' == x && y' >= y && z' == z)
  , ps.find? (λ (x', y', z') => x' == x && y' <= y && z' == z)
  , ps.find? (λ (x', y', z') => x' == x && y' == y && z' >= z)
  , ps.find? (λ (x', y', z') => x' == x && y' == y && z' <= z)
  ].any (·.isNone)

def reachable (air ps : List Pos3) : List Pos3 := Id.run do
  let air := air.eraseDups
  let mut Q := air.filter (·.visible ps)
  let mut out := []
  while not Q.isEmpty do
    let p := Q.head!
    Q := Q.tail!
    out := p :: out

    let ns := p.neighbours
      |>.filter air.elem
      |>.filter (not ∘ out.elem)
    Q := ns ++ Q

  out

def main : IO Unit := IO.interact $ λ input =>
  let ps : List Pos3 := lines input
    |>.map (·.splitOn "," |>.map (·.toNat!) |>.first3!) 

  let air := ps.mapJoin (·.neighbours) |>.filter (not ∘ ps.elem)
  let outside := reachable air ps

  s!"{air.length}, {air.count outside.elem}"