import Aoc
-- import Aoc.Lib.List
import Aoc.Lib.Array

inductive Blueprint :=
| B (ix : Nat)
    (reqs : Array <| Array Nat)
deriving Inhabited

instance : ToString Blueprint where
  toString : Blueprint → String
  | .B ix reqs => s!"⟦#{ix}: {reqs}⟧"

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

-- abbrev Nat? := Option Nat
-- abbrev Memo := Array Nat?
-- abbrev Ix := Nat

-- def W := 5 -- lane width

-- def Ix.mk (ore: Ore) (clay: Clay) (obs: Obsidian) (time : Nat) : Ix :=
--   [ore, clay, obs, time].foldr (· ||| · <<< W) 0

-- def Ix.extract (lane : Nat) (ix : Ix) := ix >>> (lane * W) &&& ((1 <<< W) - 1)

-- def Ix.ore  : Ix → Ore      := extract 0
-- def Ix.clay : Ix → Clay     := extract 1
-- def Ix.obs  : Ix → Obsidian := extract 2
-- def Ix.time : Ix → Nat      := extract 3

def ORE := 0
def CLA := 1
def OBS := 2
def GEO := 3

def Nat.ceil : Nat → Nat → Nat 
| 0, _ => 0
| _, 0 => 100
| a, b => (a + b - 1) / b

partial
def Blueprint.maxGeodes (bp : Blueprint) : IO Nat := do
  -- IO.println reqs
  -- IO.println max_robots
  go #[0, 0, 0] #[1, 0, 0, 0] 24
  where
    reqs := match bp with | B _ reqs => reqs
    max_robots := reqs |>.foldl (·.zipWith · max) #[0, 0, 0] |>.push 24
    go (res robots : Array Nat) : Nat → IO Nat 
    | 0 => return 0
    | T+1 => do
      let mut best := 0
      for i in [:4] do
        let i := 3 - i
        if robots[i]! < max_robots[i]! then 
          let wait := reqs[i]!
            |>.zipWith res Nat.sub
            |>.zipWith robots Nat.ceil
            |>.foldl max 0
          if wait ≥ T then continue
          let res := res
            |>.zipWith robots (· + · * wait)
            |>.zipWith reqs[i]! Nat.sub
            |>.zipWith robots Nat.add
          let robots := if i < 3 then robots.upd! i (· + 1) else robots
          let cur := (if i == 3 then (T - wait) else 0) + (← go res robots (T - wait))
          best := max best cur
      -- if T > 0 then
      --   IO.println (T, res, robots, best)
      return best
        

def Blueprint.quality : Blueprint → IO Nat
| bp@(.B ix _) => return ix * (← bp.maxGeodes)

-- def main : IO Unit := IO.interact $ λ input =>
def main : IO Unit := do
  let input ← IO.readInput 10000
  let blueprints := lines input |>.map Blueprint.mk

  let mut tot := 0

  for bp in blueprints do
    let cur ← bp.quality
    -- IO.println cur
    tot := tot + cur

  IO.println tot