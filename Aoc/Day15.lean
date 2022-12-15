import Aoc
import Aoc.Lib.List
import Aoc.Lib.Prod
import Aoc.Lib.Mergesort

abbrev Pos := Int × Int
abbrev Reading := Pos × Pos

def parseReading (s : String) : Reading :=
  s.splitOn ":"
  |>.map (λ w => w.splitOn "x="
                |>.get! 1
                |>.splitOn ", y="
                |>.map String.toInt!
                |>.first2!)
  |>.first2!

def dist : Reading → Int 
| ((Sx, Sy), (Bx, By)) => (Sx - Bx).natAbs + (Sy - By).natAbs

abbrev Range := Int × Int

instance : Ord Range where
  compare | (l₁, r₁), (l₂, r₂) => if l₁ == l₂ then compare r₁ r₂ else compare l₁ l₂ 

def Range.length : Range → Nat
| (l, r) => (r - l + 1).toNat

def Range.intersectsWith : Range → Range → Bool
| (l₁, r₁), (l₂, r₂) => l₁ ≤ r₂ && l₂ ≤ r₁

def Range.intersect : Range → Range → Range
| (l₁, r₁), (l₂, r₂) => (max l₁ l₂, min r₁ r₂)

def List.compress (l : List Range) : List Range := Id.run do
  let l := l.mergeSort
  let mut p := l.head!
  let mut res := []
  for q in l.drop 1 do
    if p.intersectsWith q then
      p := (p.fst, max p.snd q.snd)
    else
      res := p :: res
      p := q
  res := p :: res
  return res.reverse

def List.blockedInCol (readings: List Reading) (col: Nat) : List Range := readings
  |>.map (λ r@((Sx, Sy), _) =>
    let d := dist r - (Sy - col).natAbs
    (Sx - d, Sx + d))
  |>.filter (λ (l, r) => l <= r)
  |>.compress

def totalLength (l : List Range) : Nat := l |>.map Range.length |>.foldl .add 0

def beaconsInCol (readings: List Reading) (col: Nat) : Nat := readings
  |>.map Prod.snd
  |>.filter (λ (_, By) => By == col)
  |>.eraseDups
  |>.length

def main : IO Unit := IO.interact $ λ input =>
  let rs := lines input |>.map parseReading

  -- part 1
  let ty := 2000000
  let blocked := totalLength (rs.blockedInCol ty) - beaconsInCol rs ty

  -- part 2
  let N := 4000000
  let get (rs : List Reading) := List.range (N + 1)
    |>.find? (λ y => N + 1 > totalLength (rs.blockedInCol y |>.map (.intersect (0, N))))
    |>.get!

  let y := get rs
  let x := get (rs.map (Prod.map Prod.swap Prod.swap))
  let tuningFreq := x * N + y

  s!"{blocked}, {tuningFreq}"