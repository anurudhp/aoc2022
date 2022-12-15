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

def Grid.upd (g : Grid) (p : Pos) (c : Cell) : Id Grid := do
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

def main : IO Unit := IO.interact $ λ input =>
  let rocks := lines input |>.map parseRock

  let get (rs : List Rock) (f : Pos → Nat) := let cs := rs.map (.map f) |>.join; (cs.minimum?.get!, cs.maximum?.get!)

  let maxX := get rocks Prod.fst |>.snd
  let maxY := get rocks Prod.snd |>.snd
  let maxX := maxX * 2
  let maxY := maxY + 2

  let src := (500, 0)
  
  let (p₁, p₂) := Id.run do
    let mut grid := mkGrid maxX maxY |> rocks.foldl .addRock |>.upd src .Source
    let mut res := []
    let mut acc := 0
    for _ in [:2] do
      while true do
        let mut (x, y) := src
        while y < maxY do
          if grid.empty (x, y + 1) then
            y := y + 1
          else if x > 0 && grid.empty (x - 1, y + 1) then
            x := x - 1
            y := y + 1
          else if x + 1 < maxX && grid.empty (x + 1, y + 1) then
            x := x + 1
            y := y + 1
          else
            break
        if y ≥ maxY || (x, y) == src then break
        grid ← grid.upd (x, y) .Sand
        acc := acc + 1
      res := acc :: res
      grid := grid.addRock [(1, maxY), (maxX - 1, maxY)]
    return res.reverse.first2!

  s!"{p₁}, {p₂ + 1}"