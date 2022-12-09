import Aoc
import Aoc.Lib.List

namespace Day05

@[reducible] def Stack := List Char
inductive Move := | Move (num src dst : Nat) deriving Inhabited
@[reducible] def Input := List Stack × List Move

def parseInput (inp : String) : Input := 
  let data := inp |> lines |>.splitOn (String.isEmpty) 

  let nStacks := data.get! 0 |>.getLast! |> words |>.length -- assume: stacks numbered 1 .. nStacks
  let extract : List Char → List Char
    | [_, '[', c, ']'] => [c]
    | _ => [] 
  let readRow (row : List Char) : List (List Char) := ' ' :: row |>.toChunks 4 |>.map extract |>.padRight [] nStacks
  let stacks := data.get! 0 |>.dropLast |>.map (String.data :> readRow) |>.transpose |>.map List.join

  let readMove m := Option.get! $
    match words m with
    | ["move", n, "from", s, "to", d] => some $ Move.Move (n.toNat!) (s.toNat! - 1) (d.toNat! - 1)
    | _ => none
  let moves := data.get! 1 |>.map readMove

  (stacks, moves)


def move (rev : Bool) (ss : List Stack) (m : Move) : List Stack :=
  match m with
  | Move.Move num src dst => 
  let (hs, ts) := ss.get! src |>.splitAt num
  let hs := (if rev then hs.reverse else hs) ++ ss.get! dst
  ss |>.set src ts |>.set dst hs 

def solve (rev : Bool) : Input → String 
| (stacks, moves) => moves.foldl (move rev) stacks |>.map List.head! |> String.mk

def main (inp : String) : String := 
  let inp := parseInput inp
  s!"{solve true inp}, {solve false inp}"
