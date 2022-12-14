import Aoc
import Aoc.Lib.List

inductive Move := | R | P | S deriving BEq 

def readMove : String → Option Move
  | "A" => pure Move.R
  | "B" => pure Move.P
  | "C" => pure Move.S
  | _ => none

def Move.shape : Move → Int
  | R => 1
  | P => 2
  | S => 3

def Move.weakness : Move → Move
  | R => P
  | P => S
  | S => R

def outcome (me opp : Move) : Int :=
  if me == opp then 3
  else if me == opp.weakness then 6
  else 0

inductive Strat := | X | Y | Z deriving BEq

def readStrat : String → Option Strat
  | "X" => pure Strat.X
  | "Y" => pure Strat.Y
  | "Z" => pure Strat.Z
  | _ => none

abbrev Round := Move × Strat
abbrev Play := Move × Move

def mkRound (line : String) : Option Round := do
  let ws := words line
  return (← ws.get? 0 >>= readMove, ← ws.get? 1 >>= readStrat)

def score : Play → Int | (opp, me) => me.shape + outcome me opp

def conv1 : Round → Play
  | (o, .X) => (o, .R)
  | (o, .Y) => (o, .P)
  | (o, .Z) => (o, .S)

def conv2 : Round → Play
  | (o, .X) => (o, o.weakness.weakness)
  | (o, .Y) => (o, o)
  | (o, .Z) => (o, o.weakness)

def compute (conv : Round → Play) (rounds : List Round) : Int :=
  rounds |>.map conv |>.map score |>.foldl .add 0

def main : IO Unit := IO.interact $ λ input =>
  let rounds := input |> lines |>.map mkRound |>.catOptions
  s!"{compute conv1 rounds}, {compute conv2 rounds}"