import Aoc
import Aoc.Lib.List

def findMarker (l : Nat) (s : List Char) : Nat :=
  if (s.take l |>.eraseDups |>.length) == l then l
  else match s with 
  | _ :: s => 1 + findMarker l s
  | _ => 0

def main : IO Unit := IO.interact $ λ input =>
  let inp := input.trim.data
  let sol := (findMarker · inp)
  s!"{sol 4}, {sol 14}"

