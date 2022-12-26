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

inductive Dir := | Right | Down | Left | Up
deriving Inhabited

def Dir.rotCW : Dir → Dir
| Right => Down
| Down  => Left
| Left  => Up
| Up    => Right

def Dir.rotate (d : Dir) : Move → Dir
| .R => d.rotCW
| .L => (3 : Nat).repeat rotCW d
| _  => d

def Dir.toPasswd : Dir → Nat
| .Right => 0
| .Down  => 1
| .Left  => 2
| .Up    => 3

abbrev Pos := Nat × Nat × Dir

def Pos.toPasswd : Pos → Nat
| (x, y, d) => x * 1000 + y * 4 + d.toPasswd

def Grid.walk (g : Grid) : Pos → Pos 
| p@(x, y, d) =>
  let x' := 
    match d with
    | .Down =>
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
    | .Right =>
      if g.at x (y + 1) ≠ ' '
      then y + 1
      else g[x]!.firstIdx (· ≠ ' ') 
    | .Left  =>
      if g.at x (y - 1) ≠ ' '
      then y - 1
      else g[x]!.lastIdx (· ≠ ' ') 
    | _ => y
  if g.at x' y' == '.' then (x', y', d) else p

def Grid.move (g : Grid) : Pos → Move → Pos 
| p, .Walk n => n.repeat g.walk p
| (x, y, d), rot => (x, y, d.rotate rot)

instance : ToString Move where toString | .Walk n => s!"(Walk {n})" | .L => "(Turn L)" | .R => "(Turn R)"
instance : ToString Dir where toString | .Right => ">" | .Down => "v" | .Left => "<" | .Up => "^"

-- def main : IO Unit := IO.interact $ λ input =>
def main : IO Unit := do
  let input ← IO.readInput 10000000
  let input := lines input |>.reverse
  let moves : Array Move := mkMoves input.head!
  
  let grid := input.drop 2 |>.reverse |> Array.mk
  let m := grid.map String.length |>.foldl max 0 |>.add 1
  let grid : Grid := grid
    |>.map (λ s => " " ++ s.pushn ' ' (m - s.length))
    |>.push ("".pushn ' ' (m + 1))
    |>.insertAt 0 ("".pushn ' ' (m + 1))

  let src := (1, grid[1]!.firstIdx (· == '.'), .Right)
  let dst := moves.foldl grid.move src
  IO.println s!"{dst.toPasswd}"
