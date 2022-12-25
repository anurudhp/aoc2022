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

def Grid.mk : Grid := #[]

def Grid.tr (s t : Char) (g : Grid) : Grid := 
  g.map (·.map (λ c => if c == s then t else c))

def Grid.placeRock (g : Grid) (rock : Rock) : Grid :=
  g ++ List.replicate 3 "......." ++ rock

def Grid.compress (g : Grid) : Grid := g.popWhile (· == ".......")

def Grid.shorten (g : Grid) : Grid := Id.run do
  let n := g.size
  let mut g := g

  let mut Q := []
  for j in [0:7] do
    if g[n - 1]!.get! ⟨j⟩ == '.' then
      Q := (n - 1, j) :: Q
      g := g.upd! (n - 1) (·.set ⟨j⟩ '*')
  
  while not Q.isEmpty do
    let (i, j) := Q.head!
    Q := Q.tail!

    for (i', j') in [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)] do
      if i' >= n || j' >= 7 then continue
      if g[i']!.get! ⟨j'⟩ != '.' then continue
      g := g.upd! i' (·.set ⟨j'⟩ '*')
      Q := (i', j') :: Q

  return g |>.tr '.' '#'
    |>.tr '*' '.'
    |>.reverse
    |>.popWhile (· == "#######")
    |>.reverse

def Grid.finalize (g : Grid) : Grid := g.tr '@' '#'

inductive Dir := | Left | Right | Down deriving BEq, Inhabited

def Char.toDir : Char → Dir
| '>' => .Right
| '<' => .Left
| _   => panic "unreachable"

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

def computeGrids (jet : String) : (Array Nat × Nat) := Id.run do
  let mut ix := 0
  let mut r := 0
  let mut g := Grid.mk

  let mut gs := []
  let mut it := 1

  while true do
    let len₀ := g.size

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
    let δ := g.size - len₀
    g := g.shorten

    if let some (it', _) := gs.lookup (g, r, ix) then
      return (Array.mk <| δ :: gs.map (·.snd.snd) |>.reverse, it')
    else
      gs := ((g, r, ix), (it, δ)) :: gs

    it := it + 1

  panic "unreachable"

def Array.subsum (a : Array Nat) (l r : Nat) : Nat := a.extract l r |>.foldl .add 0

def height (δ : Array Nat) (jmp : Nat) (n : Nat) : Nat :=
  let n_chain := min n jmp
  let n := n - n_chain

  let l_cyc := δ.size - jmp
  let n_cyc := n / l_cyc
  let n := n % l_cyc

  let h_chain := δ.subsum 0 n_chain 
  let h_cyc := δ.subsum jmp δ.size
  let h_rem := δ.subsum jmp (jmp + n) 
  
  h_chain + h_cyc * n_cyc + h_rem

infixl:100 "=<<" => flip bind

def main : IO Unit := IO.interact $ λ jet => Id.run do
  let (δ, jmp) := computeGrids jet
  let h := height δ jmp
  s!"{h 2022}, {h 1000000000000}"
