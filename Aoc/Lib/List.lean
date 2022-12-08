-- https://github.com/leanprover/std4/blob/bc4c3f72b28abc028fc55ea49d827089fb7e9ef4/Std/Data/List/Basic.lean#L452-L458
def List.splitAt (n : Nat) (l : List α) : List α × List α := go l n #[] where
  /-- Auxiliary for `splitAt`: `splitAt.go l n xs acc = (acc.toList ++ take n xs, drop n xs)`
  if `n < length xs`, else `(l, [])`. -/
  go : List α → Nat → Array α → List α × List α
  | [], _, _ => (l, [])
  | x :: xs, n+1, acc => go xs n (acc.push x)
  | xs, _, acc => (acc.toList, xs)

-- https://github.com/leanprover/std4/blob/bc4c3f72b28abc028fc55ea49d827089fb7e9ef4/Std/Data/List/Basic.lean#L1452-L1467
def List.toChunks {α} : Nat → List α → List (List α)
  | _, [] => []
  | 0, xs => [xs]
  | n, x :: xs =>
    let rec
    /-- Auxliary definition used to define `toChunks`.
    `toChunks.go xs acc₁ acc₂` pushes elements into `acc₁` until it reaches size `n`,
    then it pushes the resulting list to `acc₂` and continues until `xs` is exhausted. -/
    go : List α → Array α → Array (List α) → List (List α)
    | [], acc₁, acc₂ => acc₂.push acc₁.toList |>.toList
    | x :: xs, acc₁, acc₂ =>
      if acc₁.size == n then
        go xs ((Array.mkEmpty n).push x) (acc₂.push acc₁.toList)
      else
        go xs (acc₁.push x) acc₂
    go xs #[x] #[]
  
def List.foldl1 (f : α → α → α) (l : List α) : Option α :=
  match l with
  | a :: as => pure $ as.foldl f a
  | _ => none