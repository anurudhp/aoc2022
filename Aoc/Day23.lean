import Aoc
import Aoc.Lib.Array
import Aoc.Lib.List

abbrev Pos := Nat × Nat
abbrev Grid := Array String

def Grid.area (g : Grid) : Nat := g.size * g[0]!.length

def Grid.emptyArea (g : Grid) : Nat :=
  g.map (·.data.count (· == '.')) |>.foldl .add 0

def Grid.at (g : Grid) : Pos → Char
| (i, j) => g[i]!.get! ⟨j⟩

def Grid.upd (g : Grid) : Pos → Char → Grid
| (i, j), c => g.upd! i (·.set ⟨j⟩ c)

def Grid.pad (g : Grid) : Grid :=
  let g := g.map ("." ++ · ++ ".") 
  let s := "".pushn '.' g[0]!.length
  g.push s |>.insertAt 0 s

def Grid.trim (g : Grid) : Grid := Id.run do
  let g := g 
    |>.popWhile (·.all (· == '.'))
    |>.reverse
    |>.popWhile (·.all (· == '.'))
    |>.reverse

  let m := g[0]!.length
  let mut l := m
  let mut r := m
  for row in g do
    let mut i := 0
    for c in row.data do
      if c != '.' then break
      i := i + 1
    l := min l i
    i := 0
    for c in row.data.reverse do
      if c != '.' then break
      i := i + 1
    r := min r i

  g.map (·.extract ⟨l⟩ ⟨m - r⟩)

inductive Dir := | N | S | W | E deriving BEq, Inhabited

def Grid.checkFree (g : Grid) (ps : List Pos) : Bool :=
  ps.all (g.at · == '.')

def Grid.proposeOne (g : Grid) : Pos → Dir → Option Pos
| (i, j), .N =>
  if g.checkFree <| (i - 1, ·) <$> [j - 1, j, j + 1] then
    (i - 1, j)
  else none
| (i, j), .S => 
  if g.checkFree <| (i + 1, ·) <$> [j - 1, j, j + 1] then
    (i + 1, j)
  else none
| (i, j), .W => 
  if g.checkFree <| (·, j - 1) <$> [i - 1, i, i + 1] then
    (i, j - 1)
  else none
| (i, j), .E => 
  if g.checkFree <| (·, j + 1) <$> [i - 1, i, i + 1] then
    (i, j + 1)
  else none

def Grid.propose (g : Grid) (p : Pos) (ds : List Dir) : Option Pos := do
  let (i, j) := p
  let mut ok := false
  for i' in [i - 1, i, i + 1] do
    for j' in [j - 1, j, j + 1] do
      if (i', j') ≠ p ∧ g.at (i', j') ≠ '.' then
        ok := true
  if not ok then none
  ds.firstM (g.proposeOne p)

abbrev PState := Bool × Grid × List Dir

def PState.round : PState → PState
| (_, g, ds) => Id.run do
  -- first half
  let g := g.pad
  let (n, m) := (g.size, g[0]!.length)
  let mut props := #[]
  for i in [:n] do
    for j in [:m] do
      if g.at (i, j) == '#' then
        if let some (i', j') := g.propose (i, j) ds then
          props := props.push ((i, j), (i', j'))
  let targs := props.map (·.snd)

  -- second half
  let mut g := g
  let mut stable := true
  for (p, p') in props do
    if targs.count (· == p') == 1 then
      g := g |>.upd p '.' |>.upd p' '#'
      stable := false

  let ds := ds.tail! ++ [ds.head!]
  return (stable, g.trim, ds)

def main : IO Unit := IO.interact $ λ input =>
  let grid := Array.mk <| lines input

  let ds := [.N, .S, .W, .E]

  let st@(_, grid, _) := Nat.repeat PState.round 10 (false, grid, ds)
  let area := grid.emptyArea

  let stable := Id.run do
    let mut it := 10
    let mut st := st
    while true do
      st := st.round
      it := it + 1
      if st.fst then break
    return it

  s!"{area}, {stable}"
  

