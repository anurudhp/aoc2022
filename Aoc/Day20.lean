import Aoc

abbrev Message := Array (Int × Nat)

def Message.mixₚ (xs : Message) (p : Nat) : Message := Option.get! do
  let i ← xs.findIdx? (·.snd |> (· == p))
  let (x, _) ← xs[i]?
  let xs := xs.eraseIdx i
  let i' := i + x |>.mod xs.size |>.add xs.size |>.mod xs.size |>.toNat
  xs.insertAt i' (x, p)

def Message.mix (xs : Message) : Message := List.range xs.size |>.foldl .mixₚ xs

def Message.decrypt (rounds : Nat) (xs : Message) : Int := Id.run do
  let mut xs := xs
  for _ in [:rounds] do xs := xs.mix
  let xs' := xs.map (·.fst)
  let ix₀ := xs'.getIdx? 0 |>.get!
  [1000, 2000, 3000]
    |>.map (· + ix₀ |>.mod xs.size |> xs'.get!)
    |>.foldl Int.add 0

def main : IO Unit := IO.interact $ λ input =>
  let xs := lines input |>.map String.toInt!
  let n := xs.length
  let xs : Message := Array.mk <| xs.zip <| List.range n

  -- part 1
  let dec₁ := xs.decrypt 1

  -- part 2
  let key := 811589153
  let dec₂ := xs.map (λ (x, i) => (x * key, i)) |> Message.decrypt 10
  
  s!"{dec₁}, {dec₂}"