import Aoc
import Aoc.Lib.Nat
import Aoc.Lib.Queue

def bfs (grid : Array (Array Char))
  (src : Nat × Nat × Nat) (dst : Nat × Nat) : Nat := Id.run do
  let n := grid.size
  let m := grid[0]!.size
  let (n', m') := (n - 2, m - 2)

  let T := n'.lcm m'

  let ix | (i, j, t) => (i * m + j) * T + t

  let mut Q₀ := #[(src, 0)]
  let mut Q₁ := #[]
  let mut seen := Array.mkArray (n * m * T) false

  seen := seen.set! (ix src) true

  while not Q₀.isEmpty do
    let ((i, j, t), d) := Q₀.back
    Q₀ := Q₀.pop

    if (i, j) == dst then return d

    let t' := t + 1 |>.mod T

    for (i', j') in
        [ (i, j + 1)
        , (i, j - 1)
        , (i + 1, j)
        , (i - 1, j)
        , (i, j)
        ] do
      if   i' >= n 
        || j' >= m 
        || grid[i']![j']! == '#'
        || seen[ix (i', j', t')]!
        || grid[i']![1 + (j' - 1 + t') % m']! == '<'
        || grid[i']![1 + (j' - 1 + (m' - t' % m')) % m']! == '>'
        || grid[1 + (i' + (n' - 1) + (t' % n')) % n']![j']! == '^'
        || grid[1 + (i' + (n' - 1) + (n' - t' % n')) % n']![j']! == 'v'
      then continue

      seen := seen.set! (ix (i', j', t')) true
      Q₁ := Q₁.push ((i', j', t'), d + 1)
    
    if Q₀.isEmpty then
      Q₀ := Q₁
      Q₁ := #[]
  
  panic "unreachable"

def main : IO Unit := IO.interact $ λ input =>
  let grid := lines input 
    |>.map (·.data |> Array.mk)
    |> Array.mk

  let n := grid.size
  let m := grid[0]!.size
  let T := (n - 2).lcm (m - 2)

  let δ₁ := bfs grid (0, 1, 0) (n - 1, m - 2)
  let δ₂ := δ₁ + bfs grid (n - 1, m - 2, δ₁ % T) (0, 1)
  let δ₂ := δ₂ + bfs grid (0, 1, δ₂ % T) (n - 1, m - 2)
  s!"{δ₁}, {δ₂}"
