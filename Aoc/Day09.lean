import Aoc

namespace Day09

@[reducible] def Dir := String
@[reducible] def Move := Dir × Nat

def mkMove (ws : String) : Move := 
  let ws := words ws
  (ws.head!, ws.get! 1 |>.toNat!)

@[reducible] def Pos := Int × Int -- x, y

def Pos.advance₁ : Pos → Char → Pos
| (x, y), 'U' => (x + 1, y)
| (x, y), 'D' => (x - 1, y)
| (x, y), 'R' => (x, y + 1)
| (x, y), 'L' => (x, y - 1)
| p     , _   => p

def Pos.advance (p : Pos) (dir : Dir) : Pos := Id.run do
  let mut p := p
  for d in dir.data do
    p := p.advance₁ d
  p

def Pos.dist_inf : Pos → Pos → Nat
| (x₁, y₁), (x₂, y₂) => max (x₁ - x₂).natAbs (y₁ - y₂).natAbs

def Pos.dist₁ : Pos → Pos → Nat
| (x₁, y₁), (x₂, y₂) => (x₁ - x₂).natAbs + (y₁ - y₂).natAbs

def Part := Pos × List Pos -- current, history

def Part.advance : Part → Dir → Part
| (pos, hist), dir =>
  let pos := pos.advance dir
  (pos, pos :: hist)

def State := Part × Part -- head, tail

def init : State :=
  let s := (0, 0)
  let p := (s, [s])
  (p, p)

def moveOnce : State → Dir → State 
| (hd, tl), dir => Id.run do
  let hd := hd.advance dir
  let dis := hd.fst.dist_inf tl.fst
  let mut tl' := tl
  if dis == 2 then
    for cand in ["L", "R", "U", "D", "LU", "LD", "RU", "RD"].map (tl.advance) do
      if hd.fst.dist₁ cand.fst < hd.fst.dist₁ tl'.fst then
        tl' := cand
  (hd, tl')

def State.applyMove (s : State) : Move →  State
| (_, 0) => s
| (dir, n+1) => moveOnce s dir |>.applyMove (dir, n)

def main (inp : String) : String :=
  let inp := inp |> lines |>.map mkMove

  let tailSeen := inp
    |>.foldl .applyMove init
    |>.snd -- tail
    |>.snd -- history
    |>.eraseDups 
    |>.length

  s!"{tailSeen}"