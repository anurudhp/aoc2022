def Array.upd! [Inhabited α] (ix : Nat) (f : α → α) (as : Array α) : Array α :=
  as.set! ix (f (as.get! ix))
