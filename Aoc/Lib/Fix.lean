def fixN [Inhabited α] (T : Nat) (f : Thunk α → α) : α := 
match T with
| 0 => default
| T+1 => f ⟨λ _ => (fixN T f)⟩

-- use larger recursion depth if needed
def fix [Inhabited α] : (Thunk α → α) → α := fixN 100000000