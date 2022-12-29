import Aoc
import Aoc.Lib.Array

inductive Blueprint :=
| B (ix : Nat)
    (reqs : Array <| Array Nat)
deriving Inhabited

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
    | T+1 => List.foldl max 0 <| [3, 2, 1, 0].map (λ i => 
        if (i == 3 || robots[i]! < max_robots[i]!) && (T > 2 || i == 3) then 
          let wait := reqs[i]!
            |>.zipWith res .sub
            |>.zipWith robots .ceil
            |>.foldl max 0
          if wait < T then
            if wait + 1 == T then
              if i == 3 then 1 else 0
            else
              let res' := res
                |>.zipWith robots (· + · * wait)
                |>.zipWith reqs[i]! .sub
                |>.zipWith robots .add
              let robots' := if i < 3 then robots.upd! i (· + 1) else robots
              have : T - wait < T + 1 := by sorry
              (if i == 3 then (T - wait) else 0) + go res' robots' (T - wait) 
          else 0
        else 0
      )
    -- termination_by _ _ T => T
        
def Blueprint.quality : Blueprint → Nat | bp@(.B ix _) => ix * bp.maxGeodes 24

def main : IO Unit := IO.interact $ λ input =>
  let blueprints := lines input |>.map Blueprint.mk

  let totalQuality := blueprints
    |>.map (·.quality)
    |>.foldl .add 0
  let prod := blueprints
    |>.take 3
    |>.map (·.maxGeodes 32)
    |>.foldl .mul 1

  s!"{totalQuality}, {prod}"