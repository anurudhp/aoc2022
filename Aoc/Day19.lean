import Aoc
import Aoc.Lib.List
import Aoc.Lib.Fix

abbrev Ore := Nat
abbrev Clay := Nat
abbrev Obsidian := Nat
abbrev Geode := Nat

inductive Blueprint :=
| B (ix : Nat)
    (oreR clayR : Ore)
    (obsidianR : Ore × Clay)
    (geodeR : Ore × Obsidian)
deriving Inhabited

def Blueprint.mk (s: String) : Blueprint :=
  match words s with
  | [ "Blueprint", ix
    , "Each", "ore", "robot", "costs", oreR, "ore."
    , "Each", "clay", "robot", "costs", clayR, "ore."
    , "Each", "obsidian", "robot", "costs", obsR_o, "ore", "and", obsR_c, "clay."
    , "Each", "geode", "robot", "costs", gR_o, "ore", "and", gR_obs, "obsidian."
    ] => B (ix.dropRight 1).toNat!
           oreR.toNat!
           clayR.toNat!
           (obsR_o.toNat!, obsR_c.toNat!)
           (gR_o.toNat!, gR_obs.toNat!)
  | _ => panic "failed to parse blueprint!"

abbrev Nat? := Option Nat
abbrev Memo := Array Nat?
abbrev Ix := Nat

def W := 5 -- lane width

def Ix.mk (ore: Ore) (clay: Clay) (obs: Obsidian) (time : Nat) : Ix :=
  [ore, clay, obs, time].foldr (· ||| · <<< W) 0

def Ix.extract (lane : Nat) (ix : Ix) := ix >>> (lane * W) &&& ((1 <<< W) - 1)

def Ix.ore  : Ix → Ore      := extract 0
def Ix.clay : Ix → Clay     := extract 1
def Ix.obs  : Ix → Obsidian := extract 2
def Ix.time : Ix → Nat      := extract 3

def compute (bp : Blueprint) : Ix → Memo → Memo := fix go
  where
    go (κ : Thunk (Ix → Memo → Memo)) (state: Ix) (memo:  Memo) : Memo := Id.run do
      if memo[state]!.isSome then return memo
      if state.time == 0 then return memo

      memo

def main : IO Unit := IO.interact $ λ input =>
  let blueprints := lines input |>.map Blueprint.mk
  let sz := 1 <<< (3 * W + 5)
  s!"TODO"