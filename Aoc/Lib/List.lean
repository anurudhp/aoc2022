-- https://github.com/leanprover/std4/blob/bc4c3f72b28abc028fc55ea49d827089fb7e9ef4/Std/Data/List/Basic.lean#L452-L458
def List.splitAt (n : Nat) (l : List α) : List α × List α := go l n #[] where
  /-- Auxiliary for `splitAt`: `splitAt.go l n xs acc = (acc.toList ++ take n xs, drop n xs)`
  if `n < length xs`, else `(l, [])`. -/
  go : List α → Nat → Array α → List α × List α
  | [], _, _ => (l, [])
  | x :: xs, n+1, acc => go xs n (acc.push x)
  | xs, _, acc => (acc.toList, xs)

def List.splitOn (pred : α → Bool) (l: List α) : List (List α) := go l where
  go : List α → List (List α)
  | [] => [[]]
  | x::xs =>
    match go xs with
    | [] => [] -- unreachable
    | p::ps => if pred x then [] :: p :: ps else (x :: p) :: ps

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
  
def List.foldl1 (f : α → α → α) : List α → Option α
| a :: as => some $ as.foldl f a
| _ => none

def List.padRight (a : α) (n : Nat) (l : List α) :=
  l ++ List.replicate (n - l.length) a

-- https://github.com/leanprover/std4/blob/bc4c3f72b28abc028fc55ea49d827089fb7e9ef4/Std/Data/List/Basic.lean#L848-L868
def List.transpose (l : List (List α)) : List (List α) := (l.foldr go #[]).toList where
  /-- `pop : List α → StateM (List α) (List α)` transforms the input list `old`
  by taking the head of the current state and pushing it on the head of `old`.
  If the state list is empty, then `old` is left unchanged. -/
  pop (old : List α) : StateM (List α) (List α)
    | [] => (old, [])
    | a :: l => (a :: old, l)

  /-- `go : List α → Array (List α) → Array (List α)` handles the insertion of
  a new list into all the lists in the array:
  `go [a, b, c] #[l₁, l₂, l₃] = #[a::l₁, b::l₂, c::l₃]`.
  If the new list is too short, the later lists are unchanged, and if it is too long
  the array is extended:
  ```
  go [a] #[l₁, l₂, l₃] = #[a::l₁, l₂, l₃]
  go [a, b, c, d] #[l₁, l₂, l₃] = #[a::l₁, b::l₂, c::l₃, [d]]
  ```
  -/
  go (l : List α) (acc : Array (List α)) : Array (List α) :=
    let (acc, l) := acc.mapM pop l
    l.foldl (init := acc) fun arr a => arr.push [a]

def List.catOptions : List (Option α) → List α
| [] => []
| x :: xs => 
  let xs := xs.catOptions
  if let some v := x then v :: xs else xs

def List.first2! [Inhabited α] : List α → α × α
| x :: y :: _ => (x, y)
| _ => panic "List.first2!: list does not contain 2 elements"

def List.first3! [Inhabited α] : List α → α × α × α
| x :: y :: z :: _ => (x, y, z)
| _ => panic "List.first3!: list does not contain 3 elements"

def List.count (p : α → Bool) : List α → Nat := (·.filter p |>.length)