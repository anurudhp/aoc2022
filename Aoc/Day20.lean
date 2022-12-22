import Aoc

abbrev Message := Array (Int × Nat)

def Message.mix (xs : Message) (p : Nat) : Message := Option.get! do
  let i ← xs.findIdx? (·.snd |> (· == p))
  let (x, _) ← xs[i]?
  let xs := xs.eraseIdx i
  let i' := i + x |>.mod xs.size |>.add xs.size |>.mod xs.size |>.toNat
  xs.insertAt i' (x, p)

def main : IO Unit := IO.interact $ λ input =>
  let xs := lines input |>.map String.toInt!
  let n := xs.length
  let xs : Message := Array.mk <| xs.zip <| List.range n

  let xs' := List.range n |>.foldl Message.mix xs |>.map (·.fst)
  let ix₀ := xs'.getIdx? 0 |>.get!
  let dec₁ := [1000, 2000, 3000]
    |>.map (· + ix₀ |>.mod n |> xs'.get!)
    |>.foldl Int.add 0
  
  s!"{dec₁}"