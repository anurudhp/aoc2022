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

@[reducible] def Part := Pos × List Pos -- current, history

def Part.advance : Part → Dir → Part
| (pos, hist), dir =>
  let pos := pos.advance dir
  (pos, pos :: hist)

@[reducible] def Snake := List Part

def init (len : Nat) : Snake :=
  let s := (0, 0)
  let p := (s, [s])
  .replicate len p

def Snake.moveOnce : Snake → Dir → Snake 
| snake, dir => Id.run do
  let mut hd := snake.head!.advance (dir ++ dir) -- fake head to pull the real head
  let mut snake' := []
  for pt in snake do
    let dis := hd.fst.dist_inf pt.fst
    let mut pt' := pt
    if dis == 2 then
      for cand in ["L", "R", "U", "D", "LU", "LD", "RU", "RD"].map (pt.advance) do
        if hd.fst.dist₁ cand.fst < hd.fst.dist₁ pt'.fst then
          pt' := cand
    snake' := pt' :: snake'
    hd := pt'
  snake'.reverse

def Snake.applyMove (s : Snake) : Move → Snake
| (_, 0) => s
| (dir, n+1) => s.moveOnce dir |>.applyMove (dir, n)

def main (inp : String) : String :=
  let inp := inp |> lines |>.map mkMove

  let tailSeen (len : Nat) := inp
    |>.foldl .applyMove (init len)
    |>.getLast! -- tail
    |>.snd -- history
    |>.eraseDups 
    |>.length

  s!"{tailSeen 2}, {tailSeen 10}"