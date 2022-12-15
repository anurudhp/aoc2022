import Aoc
import Aoc.Lib.List
import Aoc.Lib.Mergesort

abbrev Pos := Int × Int
abbrev Reading := Pos × Pos

def parse (s : String) : Reading :=
  s.splitOn ":"
  |>.map (λ w => w.splitOn "x="
                |>.get! 1
                |>.splitOn ", y="
                |>.map String.toInt!
                |>.first2!)
  |>.first2!

def dist : Reading → Int 
| ((Sx, Sy), (Bx, By)) => (Sx - Bx).natAbs + (Sy - By).natAbs

abbrev Range := Int × Int

instance : Ord Range where
  compare | (l₁, r₁), (l₂, r₂) => if l₁ == l₂ then compare r₁ r₂ else compare l₁ l₂ 

def Range.intersect? : Range → Range → Bool
| (l₁, r₁), (l₂, r₂) => l₁ ≤ r₂ && l₂ ≤ r₁

def totalLength (l : List Range) : Nat := flip Option.getD 0 do
  let l := l.mergeSort
  let mut p ← l.head?
  let mut len := 0
  for q in l.drop 1 do
    if p.intersect? q then
      p := (p.fst, max p.snd q.snd)
    else
      len := len + (p.snd - p.fst + 1).natAbs
      p := q
  len := len + (p.snd - p.fst + 1).natAbs
  return len

def main : IO Unit := IO.interact $ λ input =>
  let readings := lines input |>.map parse

  -- part 1
  let Ty := 2000000
  let invLen :=
    readings.map (λ r@((Sx, Sy), _) =>
      let d := dist r - (Sy - Ty).natAbs
      (Sx - d, Sx + d))
    |>.filter (λ (l, r) => l <= r)
    |> totalLength
  let beacs := readings.map Prod.snd |>.filter (λ (_, By) => By == Ty) |>.eraseDups |>.length
  let part1 := invLen - beacs

  s!"{part1}"