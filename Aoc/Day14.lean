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

def Grid.set (g : Grid) (p : Pos) (c : Cell) : Grid := Id.run do
  let (x, y) := p
  let mut row := g.get! x
  row := row.set! y c
  g.set! x row

def Grid.addRock (g : Grid) (r : Rock) : Grid := Id.run do
  let mut (px, py) := r.head!
  let mut g := g
  for (qx, qy) in r.drop 1 do
    for x in mkRange px qx do
      for y in mkRange py qy do
        g := g.set (x, y) .Rock
    (px, py) := (qx, qy)
  return g

def Grid.show : Grid → String
| g => g.data.map (λ r => r.data) |>.transpose |>.map (λ r => r.map toString |> String.join) |> unlines

def main : IO Unit := IO.interact $ λ input =>
  let rocks := lines input |>.map parseRock

  let get (rs : List Rock) (f : Pos → Nat) := let cs := rs.map (.map f) |>.join; (cs.minimum?.get!, cs.maximum?.get!)

  let minX := get rocks Prod.fst |>.fst
  let rocks := rocks.map (.map (λ (x, y) => (x - minX, y)))

  let maxX := get rocks Prod.fst |>.snd
  let maxY := get rocks Prod.snd |>.snd
  
  let grid := mkGrid maxX maxY |> rocks.foldl .addRock |>.set (500 - minX, 0) .Source

  s!"{grid.show}"