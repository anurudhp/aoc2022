import Aoc
import Aoc.Lib.Array

inductive Blueprint :=
| B (ix : Nat)
    (reqs : Array <| Array Nat)
deriving Inhabited

instance : ToString Blueprint where
  toString : Blueprint → String | .B ix reqs => s!"⟦#{ix}: {reqs}⟧"

def Blueprint.mk (s: String) : Blueprint :=
  match words <| s.replace ":" "" with
  | [ "Blueprint", ix
    , "Each", "ore", "robot", "costs", oreR, "ore."
    , "Each", "clay", "robot", "costs", clayR, "ore."
    , "Each", "obsidian", "robot", "costs", obsR_o, "ore", "and", obsR_c, "clay."
    , "Each", "geode", "robot", "costs", gR_o, "ore", "and", gR_obs, "obsidian."
    ] => B ix.toNat!
           #[#[oreR.toNat!, 0, 0]
           , #[clayR.toNat!, 0, 0]
           , #[obsR_o.toNat!, obsR_c.toNat!, 0]
           , #[gR_o.toNat!, 0, gR_obs.toNat!]
           ]
  | _ => panic "failed to parse blueprint!"

def ORE := 0
def CLA := 1
def OBS := 2
def GEO := 3

def Nat.ceil : Nat → Nat → Nat 
| 0, _ => 0
| _, 0 => 100
| a, b => (a + b - 1) / b

partial
def Blueprint.maxGeodes (bp : Blueprint) (time : Nat) : Nat := go #[0, 0, 0] #[1, 0, 0] time
  where
    reqs := match bp with | B _ reqs => reqs
    max_robots := reqs |>.foldl (·.zipWith · max) #[0, 0, 0]
    go (res robots : Array Nat) : Nat → Nat 
    | 0 => 0
    | 1 => 0
    | T+1 => Id.run do
      let mut best := 0
      for i in [:4] do
        let i := 3 - i
        if (i == 3 || robots[i]! < max_robots[i]!) then 
          let wait := reqs[i]!
            |>.zipWith res .sub
            |>.zipWith robots .ceil
            |>.foldl max 0
          if wait ≥ T then continue
          if wait + 1 == T then
            if i == 3 then best := max best 1
            continue
          let res := res
            |>.zipWith robots (· + · * wait)
            |>.zipWith reqs[i]! .sub
            |>.zipWith robots .add
          let robots := if i < 3 then robots.upd! i (· + 1) else robots
          best := max best <| (if i == 3 then (T - wait) else 0) + go res robots (T - wait)
      best
        
def Blueprint.quality : Blueprint → Nat | bp@(.B ix _) => ix * bp.maxGeodes 24

def main : IO Unit := IO.interact $ λ input =>
  let blueprints := lines input |>.map Blueprint.mk

  let totalQuality := blueprints.map (·.quality) |>.foldl .add 0

  s!"{totalQuality}"