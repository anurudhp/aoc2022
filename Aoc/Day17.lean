import Aoc
import Aoc.Lib.Array

abbrev Pos := Nat × Nat

abbrev Rock := List String 

def shapes : List Rock := .map (·.trim |> lines |>.reverse) ["
..@@@@.
","
...@...
..@@@..
...@...
","
....@..
....@..
..@@@..
","
..@....
..@....
..@....
..@....
","
..@@...
..@@...
"]

abbrev Grid := Array String

instance : ToString Grid where
  toString g := g.data |>.reverse |> unlines

def Grid.mk : Grid := #[]

def Grid.placeRock (g : Grid) (rock : Rock) : Grid :=
  g ++ List.replicate 3 "......." ++ rock

def Grid.compress (g : Grid) : Grid := g.popWhile (· == ".......")

def Grid.shorten (g : Grid) : Grid × Nat := (g, 0)

def Grid.finalize (g : Grid) : Grid :=
  g.map (·.map (λ c => if c == '@' then '#' else c))

inductive Dir := | Left | Right | Down deriving BEq

def Char.toDir : Char → Dir
| '>' => .Right
| '<' => .Left
| _   => .Down

def Grid.push (g : Grid) (dir : Dir) : Option Grid := do
  let g := g.compress
  let val i j := g.get! i |>.get! ⟨j⟩
  let mut rock := []
  for i in [:g.size] do
    for j in [:7] do
      if val i j == '@' then
        if match dir with
           | .Left  => j == 0 || val i (j - 1) == '#'
           | .Right => j == 6 || val i (j + 1) == '#'
           | .Down  => i == 0 || val (i - 1) j == '#'
        then
          none
        rock := (i, j) :: rock
  
  let mut g := g
  for (i, j) in rock do
    g := g.upd! i (·.set ⟨j⟩ '.')
  for (i, j) in rock do
    let i := if dir == .Down then i - 1 else i
    let j := match dir with
             |.Left  => j - 1
             |.Right => j + 1
             |.Down  => j
    g := g.upd! i (·.set ⟨j⟩ '@')
  return g.compress

def Grid.push! (g: Grid) (dir : Dir) : Grid := g.push dir |>.getD g

def main : IO Unit := IO.interact $ λ jet => Option.get! do
  let mut ix := 0
  let mut r := 0
  let mut g := Grid.mk

  let mut len := 0
  for _ in [:2022] do
    g := g.placeRock <| shapes.get! r
    r := r + 1 |>.mod shapes.length

    while true do
      g := g.push! <| jet.get! ⟨ix⟩ |>.toDir
      ix := ix + 1 |>.mod jet.length

      if let some g' := g.push .Down then
        g := g'
      else 
        break
    g := g.finalize.compress

    let (g', l) := g.shorten
    len := len + l
    g := g'
  
  s!"{len + g.size}"