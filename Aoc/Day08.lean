import Aoc
import Aoc.Lib.List
import Aoc.Lib.Mergesort

abbrev Cell := Nat × Nat 
abbrev Row := List Cell
abbrev Grid := List Row

def List.mapJoin (f : α → List β) (g : List α) : List β := g.map f |>.join

-- part 1
def checkRow (lim : Nat) : Row → List Nat
| [] => []
| (t, ix) :: ts => 
  let res := checkRow (max lim (t + 1)) ts
  if t >= lim then ix :: res else res

-- part 2
def calcDist (row : Row) : Row := Id.run do
  let mut stk := [(10, 0)]
  let mut i := 0
  let mut ans := []
  for (t, ix) in row do
    while not stk.isEmpty && stk.head!.fst < t do
      stk := stk.drop 1
    ans := (ix, i - stk.head!.snd) :: ans
    stk := (t, i) :: stk
    i := i + 1
  return ans

def main : IO Unit := IO.interact $ λ input =>
  let inp := lines input |>.map (String.data :> List.map (λ c => c.toNat - '0'.toNat))
  let n := inp.length
  let m := inp.head!.length

  let grid := inp.zipWith (λ row r => row.zipWith (λ cell c => (cell, r * m + c)) (.range m)) (.range n)

  let views := [grid, grid.transpose, grid.map .reverse, grid.transpose.map .reverse]

  let visible := views
    |>.mapJoin (.mapJoin (checkRow 0))
    |>.eraseDups
    |>.length

  let bestScenic := views
    |>.mapJoin (.mapJoin calcDist)
    |>.mergeSortBy (λ (i, _) (j, _) => i < j)
    |>.groupBy (λ (i, _) (j, _) => i == j)
    |>.map (.map Prod.snd)
    |>.map (List.foldl .mul 1)
    |>.foldl Nat.max 0

  s!"{visible}, {bestScenic}"