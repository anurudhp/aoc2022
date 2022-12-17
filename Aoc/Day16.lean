import Aoc
import Aoc.Lib.Array
import Aoc.Lib.List

abbrev Nat? := Option Nat

-- support for bitmasks 
abbrev Mask := Nat

def List.toMask (l : List Nat) : Mask :=
  l.map (1 <<< ·) |>.foldl (· ||| ·) 0

def Mask.test (n : Mask) (i : Nat) : Bool := (n >>> i) &&& 1 != 0
def Mask.set  (n : Mask) (i : Nat) : Mask := n ||| (1 <<< i)
def Mask.flip (n : Mask) (i : Nat) : Mask := n ^^^ (1 <<< i)

-- valves
inductive Valve := | V (name : Nat) (rate : Nat) (tunnels : List Nat) deriving Inhabited

def parseInput (input : String) : Array Valve × Nat := Id.run do
  let input := lines input
  let n := input.length

  let mut ixs := []
  let mut ls := []

  let mut (lix, rix) := (0, n - 1)
  for s in input do
    let s := s.splitOn "Valve " |>.get! 1
    let (name, s) := s.splitOn " has flow rate=" |>.first2!
    let (rate, s) := s.splitOn ";" |>.first2!
    let tunnels := s.trim.splitOn " "
      |>.drop 4 -- tunnel(s) lead(s) to valve(s)
      |> String.intercalate ""
      |>.splitOn ","
    
    let rate := rate.toNat!

    if rate == 0 then
      ixs := (name, rix) :: ixs
      rix := rix - 1
    else
      ixs := (name, lix) :: ixs
      lix := lix + 1

    ls := (name, rate, tunnels) :: ls
      
  let mut valves := Array.mkArray n default
  for (name, rate, tunnels) in ls do
    let name := ixs.lookup name |>.get!
    let tunnels := tunnels.map (ixs.lookup · |>.get!)
    valves := valves.set! name (.V name rate tunnels)
  return (valves, ixs.lookup "AA" |>.get!)

instance : ToString Valve where
  toString | Valve.V n r ts => s!"\{{n} --({r})--> {ts}}"

def Valve.name    : Valve → Nat      | V name _    _  => name
def Valve.rate    : Valve → Nat      | V _    rate _  => rate
def Valve.tunnels : Valve → List Nat | V _    _    ts => ts

-- accumulate, applying `f` when possible
def Option.acc (f : α → α → α) : Option α → Option α → Option α 
| some a, some b => some $ f a b
| none, b => b
| a, none => a

abbrev Array2D α := Array (Array α)

def FloydWarshall (valves : Array Valve) : Array2D Nat? := Id.run do
  let n := valves.size
  let mut dis := .mkArray n <| .mkArray n none

  for i in [:n] do
    dis := dis.upd! i (·.set! i (some 0))

  for valve in valves do
    let u := valve.name
    for v in valve.tunnels do
      dis := dis.upd! u (·.set! v (some 1))

  for k in [:n] do
    for i in [:n] do
      for j in [:n] do
        let d := Nat.add <$> dis[i]![k]! <*> dis[k]![j]!
        let d := d.acc min dis[i]![j]!
        dis := dis.upd! i (·.set! j d)
  
  return dis

-- memoization
abbrev Memo := Array Nat?
def M := 15
def logM := 4
-- dp[M][1 << M][30]
--    4    15     5  bits
abbrev Ix := Nat

def Ix.node : Ix → Nat  | i => i >>> 20
def Ix.mask : Ix → Mask | i => (i >>> 5) &&& ((1 <<< 15) - 1)
def Ix.time : Ix → Nat  | i => (i &&& 31)
def Ix.mk : Nat → Mask → Nat → Ix | u, m, t => (u <<< 20) ||| (m <<< 5) ||| t

def dfs (valves : Array Valve) (δ : Array2D Nat?) (M : Nat)
        (memo : Memo) (u : Nat) (mask : Mask)
        : Nat → Memo
| 0 => memo
| t+1 => Id.run do
  let ix := Ix.mk u mask (t + 1)
  if memo[ix]!.isSome then
    return memo
  let mut memo := memo
  let mask := mask.flip u

  let mut res := 0
  for v in [:M] do
    if not $ mask.test v then continue
    if let some d := δ[u]![v]! then
      if t - d <= 1 then continue
      let ix' := Ix.mk v mask (t - d)
      if memo[ix']!.isNone then
        have : t - d < t + 1 := by sorry
        memo := dfs valves δ M memo v mask (t - d)
      res := max res $ memo[ix']!.getD 0
  res := res + t * valves[u]!.rate
  memo.set! ix res
termination_by _ _ _ _ _ _ t => t

def main : IO Unit := IO.interact $ λ input =>
  let (valves, src) := parseInput input
  let n := valves.size
  let m := valves.count (·.rate > 0) -- [0..m-1]
  let δ := FloydWarshall valves

  Id.run do
    let mut memo : Memo := .mkArray (1 <<< 24) none
    let mut best1 := 0
    let mut bestM2 := Array.mkArray (1 <<< m) 0
    for u in [:m] do
      if let some d := δ[src]![u]! then
        for mask in [: 1 <<< m] do
          let mask := .set mask u
          for st in [30, 26] do
            memo := dfs valves δ m memo u mask (st - d)
          best1 := max best1 $ memo[Ix.mk u mask (30 - d)]!.getD 0
          bestM2 := bestM2.upd! mask (max $ memo[Ix.mk u mask (26 - d)]!.getD 0)
    
    let mut best2 := 0
    for mask in [: 1 <<< m] do
      best2 := max best2 $ bestM2[mask]! + bestM2[(1 <<< m) - 1 - mask]!
    s!"{best1}, {best2}"
