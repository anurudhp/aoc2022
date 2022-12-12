import Aoc
import Aoc.Lib.IntList
import Aoc.Lib.List

def RuckSack := List Char × List Char

def mkRuckSack (s : String) : RuckSack := s.data.splitAt (s.length / 2)

def priority (c : Char) : Int := Int.ofNat $
  if c.isLower then c.toNat - 'a'.toNat + 1
  else c.toNat - 'A'.toNat + 27 

def common [BEq α] (xs ys : List α) : List α := 
  match xs with
  | [] => [] 
  | x :: xs =>
    let rs := common xs ys
    if ys.elem x then x :: rs else rs 

def part1 : List String → Int :=
  .map (mkRuckSack
        :> uncurry common
        :> List.head!
        :> priority)
  :> sum

def part2 : List String → Int :=
  .map String.data
  :> .toChunks 3
  :> .map (List.foldl1 common
           :> Option.get!
           :> List.head!
           :> priority)
  :> sum

def main : IO Unit := IO.interact $ λ input =>
  let input := lines input
  s!"{part1 input}, {part2 input}"