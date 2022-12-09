import Aoc
import Aoc.Lib.List

namespace Day08

def rangeToList (r : Std.Range) : List Nat := Id.run do
  let mut xs := []
  for i in r do
    xs := i :: xs
  xs.reverse

instance : Coe Std.Range (List Nat) := ⟨rangeToList⟩

@[reducible] def Cell := Nat × Nat 
@[reducible] def Row := List Cell
@[reducible] def Grid := List Row

def checkRow (lim : Nat) : Row → List Nat
| [] => []
| (t, ix) :: ts => 
  let res := checkRow (max lim (t + 1)) ts
  if t >= lim then ix :: res else res

def check (g : Grid) : List Nat := g.map (checkRow 0) |>.join

def main (inp : String) : String :=
  let inp := lines inp |>.map (String.data :> List.map (λ c => c.toNat - '0'.toNat))
  let n := inp.length
  let m := inp.head!.length

  let inp := inp.zipWith (λ row r => row.zipWith (λ cell c => (cell, r * m + c)) [:m]) [:n] 

  let visible := check inp 
              ++ check inp.transpose
              ++ check (inp.map List.reverse)
              ++ check (inp.transpose.map List.reverse)
    |>.eraseDups
    |>.length

  s!"{visible}"