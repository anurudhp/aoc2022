import Aoc
import Aoc.Lib.Array

def String.firstIdx (s : String) (p : Char → Bool) : Nat := s.find p |>.byteIdx
def String.lastIdx  (s : String) (p : Char → Bool) : Nat := s.revFind p |>.get! |>.byteIdx

def Array.findIdxRev? (a : Array α) (p : α → Bool) : Option Nat := do
  for i in [:a.size] do
    let j := a.size - 1 - i
    if p (← a[j]?) then return j
  none

abbrev Grid := Array String

def Grid.at (g : Grid) (x y : Nat) : Char := g[x]!.get! ⟨y⟩

inductive Move := | Walk (n : Nat) | L | R

def mkMoves (s : String) : Array Move := Id.run do
  let mut n := 0
  let mut moves := #[]
  for c in s.data do
    if c.isDigit then
      n := n * 10 + (c.toNat - '0'.toNat)
    else
      moves := moves.push <| .Walk n 
      n := 0
      moves := moves.push <| if c == 'L' then .L else .R

  moves.push <| .Walk n 

inductive Dir := | Rt | Dn | Lt | Up

def Dir.rotCW : Dir → Dir
| Rt => Dn
| Dn => Lt
| Lt => Up
| Up => Rt

def Dir.rotate (d : Dir) : Move → Dir
| .R => d.rotCW
| .L => (3 : Nat).repeat rotCW d
| _  => d

def Dir.toPasswd : Dir → Nat
| .Rt => 0
| .Dn => 1
| .Lt => 2
| .Up => 3

abbrev Pos := Nat × Nat × Dir

def Pos.toPasswd : Pos → Nat
| (x, y, d) => x * 1000 + y * 4 + d.toPasswd

def Grid.next₁ (g : Grid) : Pos → Pos 
| (x, y, d) =>
  let x' := 
    match d with
    | .Dn =>
      if g.at (x + 1) y ≠ ' '
      then x + 1
      else g.findIdx? (·.get ⟨y⟩ ≠ ' ') |>.get!
    | .Up => 
      if g.at (x - 1) y ≠ ' '
      then x - 1
      else g.findIdxRev? (·.get ⟨y⟩ ≠ ' ') |>.get!
    | _ => x
  let y' := 
    match d with
    | .Rt =>
      if g.at x (y + 1) ≠ ' '
      then y + 1
      else g[x]!.firstIdx (· ≠ ' ') 
    | .Lt  =>
      if g.at x (y - 1) ≠ ' '
      then y - 1
      else g[x]!.lastIdx (· ≠ ' ') 
    | _ => y
  (x', y', d)

/- cube structure:
.AB
.C.
DE.
F..  -/

macro_rules | `($x ∈ [ $l ... $r ]) => `($l ≤ $x && $x ≤ $r)

def Grid.next₂ (g : Grid) : Pos → Pos | p@(x, y, d) => Id.run do
  if let .Up := d then
    if x == 1   && y ∈ [ 51 ... 100] then return (y + 100,       1, .Rt) -- A → F
    if x == 1   && y ∈ [101 ... 150] then return (    200, y - 100, .Up) -- B → F
    if x == 101 && y ∈ [  1 ...  50] then return ( y + 50,      51, .Rt) -- D → C

  if let .Dn := d then
    if x == 50  && y ∈ [101 ... 150] then return ( y - 50,     100, .Lt) -- B → C
    if x == 150 && y ∈ [ 51 ... 100] then return (y + 100,      50, .Lt) -- E → F
    if x == 200 && y ∈ [  1 ...  50] then return (      1, y + 100, .Dn) -- F → B
    
  if let .Rt := d then
    if x ∈ [  1 ...  50] && y == 150 then return (151 - x,     100, .Lt) -- B → E
    if x ∈ [ 51 ... 100] && y == 100 then return (     50,  x + 50, .Up) -- C → B
    if x ∈ [101 ... 150] && y == 100 then return (151 - x,     150, .Lt) -- E → B
    if x ∈ [151 ... 200] && y == 50  then return (    150, x - 100, .Up) -- F → E

  if let .Lt := d then
    if x ∈ [101 ... 150] && y == 1   then return (151 - x,      51, .Rt) -- D → A
    if x ∈ [151 ... 200] && y == 1   then return (      1, x - 100, .Dn) -- F → A
    if x ∈ [ 51 ... 100] && y == 51  then return (    101,  x - 50, .Dn) -- C → D
    if x ∈ [  1 ...  50] && y == 51  then return (151 - x,       1, .Rt) -- A → D

  g.next₁ p

def Grid.step (g : Grid) (next : Pos → Pos) (p : Pos) : Pos :=
  let p'@(x, y, _) := next p
  if g.at x y == '.' then p' else p

def Grid.play (g : Grid) (next : Pos → Pos) : Pos → Move → Pos 
| p, .Walk n => n.repeat (g.step next) p
| (x, y, d), rot => (x, y, d.rotate rot)

def main : IO Unit := IO.interact $ λ input =>
  let input := lines input |>.reverse
  let moves : Array Move := mkMoves input.head!
  
  let grid := input.drop 2 |>.reverse |> Array.mk
  let m := grid.map String.length |>.foldl max 0 |>.add 1
  let grid : Grid := grid
    |>.map (λ s => " " ++ s.pushn ' ' (m - s.length))
    |>.push ("".pushn ' ' (m + 1))
    |>.insertAt 0 ("".pushn ' ' (m + 1))

  let src := (1, grid[1]!.firstIdx (· == '.'), .Rt)
  let passwd next := moves.foldl (grid.play next) src |>.toPasswd
  s!"{passwd grid.next₁}, {passwd grid.next₂}"