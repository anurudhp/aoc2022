import Aoc
import Aoc.Lib.List

namespace Day06

def findMarker (l : Nat) (s : List Char) : Nat :=
  if (s.take l |>.eraseDups |>.length) == l then l
  else match s with 
  | _ :: s => 1 + findMarker l s
  | _ => 0

def main (inp : String) : String := 
  let inp := inp.trim.data
  s!"{findMarker 4 inp}, {findMarker 14 inp}"

