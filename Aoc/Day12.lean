import Aoc
import Aoc.Lib.Queue

@[reducible]
def Pos := Nat × Nat

@[reducible]
def Grid := List String

def Grid.locate (grid : Grid) (c : Char) : Option Pos := do
  let mut i := 0
  for row in grid do
    let j := row.find (BEq.beq c) |>.byteIdx
    if j != row.length then
      return (i, j)
    i := i + 1
  none

def Grid.at (grid : Grid) : Pos → Nat
| (x, y) => grid
  |>.get! x |>.get (String.Pos.mk y) 
  |> (λ c => match c with
             | 'S' => 'a'
             | 'E' => 'z'
             | c   => c)
  |>.toNat
  |>.sub ('a'.toNat)

def bfs (grid : Grid) (s e : Pos) : Nat := Option.get! do
  let n := grid.length
  let m := grid.head!.length

  let neigh : Pos → List Pos
  | u@(x, y) => Id.run do
    let mut res := []
    if x > 0     then res := (x - 1, y) :: res
    if y > 0     then res := (x, y - 1) :: res
    if x + 1 < n then res := (x + 1, y) :: res
    if y + 1 < m then res := (x, y + 1) :: res
    res.filter (λ v => grid.at v <= grid.at u + 1)

  let mut q := Queue.empty.enqueue (s, 0)
  let mut seen := [s]
  while not q.isEmpty do
    let ((u, d), q') := q.dequeue?.get!
    q := q'

    if u == e then return d

    for v in neigh u do
      if not $ seen.elem v then
        q := q.enqueue (v, d + 1)
        seen := v :: seen
  none

def main : IO Unit := IO.interact $ λ input =>
  let grid : Grid := lines input 

  let s := grid.locate 'S' |>.get!
  let e := grid.locate 'E' |>.get!

  s!"{bfs grid s e}"