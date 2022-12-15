import Aoc
import Aoc.Lib.List

abbrev Pos := Nat × Nat
abbrev Rock := List Pos

def parseRock (s : String) : Rock :=
  s.splitOn " -> "
  |>.map (λ s => s.splitOn ","
                 |>.map String.toNat!
                 |>.first2!)

inductive Cell := | Source | Rock | Sand | Empty deriving BEq, Inhabited

instance : ToString Cell where
  toString
  | .Source => "+"
  | .Rock => "#"
  | .Sand => "o"
  | .Empty => "."

abbrev Grid := Array (Array Cell)

def mkGrid (maxX maxY : Nat) : Grid := .mkArray (1 + maxX) (.mkArray (1 + maxY) .Empty)

-- [min u v ... max u v]
def mkRange (u v : Nat) : List Nat := List.range (1 + max u v) |>.drop (min u v)

def Grid.upd (g : Grid) (p : Pos) (c : Cell) : Grid := Id.run do
  let (x, y) := p
  let mut row := g.get! x
  row := row.set! y c
  g.set! x row

def Grid.at (g : Grid) : Pos → Cell
| (x, y) => g |>.get! x |>.get! y

def Grid.empty (g: Grid) (p: Pos) : Bool := g.at p == .Empty
def Grid.blocked (g: Grid) (p: Pos) : Bool := not $ g.empty p

def Grid.addRock (g : Grid) (r : Rock) : Grid := Id.run do
  let mut (px, py) := r.head!
  let mut g := g
  for (qx, qy) in r.drop 1 do
    for x in mkRange px qx do
      for y in mkRange py qy do
        g := g.upd (x, y) .Rock
    (px, py) := (qx, qy)
  return g

def Grid.show : Grid → String
| g => g.data.map (λ r => r.data) |>.transpose |>.map (λ r => r.map toString |> String.join) |> unlines

abbrev Done := true
abbrev Continue := false

def Grid.step (g : Grid) (maxX maxY : Nat) (src : Pos) : Bool × Grid := Id.run do
  let mut g := g
  let mut (x, y) := src
  while y < maxY do
    if g.empty (x, y + 1) then
      y := y + 1
    else if x > 0 && g.empty (x - 1, y + 1) then
      x := x - 1
      y := y + 1
    else if x + 1 < maxX && g.empty (x + 1, y + 1) then
      x := x + 1
      y := y + 1
    else
      break
  if y < maxY then
    g := g.upd (x, y) .Sand
    return (Continue, g)
  else
    return (Done, g)

def main : IO Unit := IO.interact $ λ input =>
  let rocks := lines input |>.map parseRock

  let get (rs : List Rock) (f : Pos → Nat) := let cs := rs.map (.map f) |>.join; (cs.minimum?.get!, cs.maximum?.get!)

  let minX := get rocks Prod.fst |>.fst
  let rocks := rocks.map (.map (λ (x, y) => (x - minX, y)))

  let maxX := get rocks Prod.fst |>.snd
  let maxY := get rocks Prod.snd |>.snd

  let src := (500 - minX, 0)
  
  let grid := mkGrid maxX maxY |> rocks.foldl .addRock |>.upd src .Source

  let (accum, grid') := Id.run do
    let mut iters := 0
    let mut grid := grid
    while true do
      let (done, grid') := grid.step maxX maxY src
      if done then break
      grid := grid'
      iters := iters + 1
    (iters, grid)

  s!"{accum}\n{grid'.show}"