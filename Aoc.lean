-- Helpers

notation:1000 f ":>" g => g ∘ f

def lines (s : String) : List String := String.splitOn s "\n"
def words (s : String) : List String := String.split s (λ c => c == ' ' || c == '\n') 

def unlines : List String -> String := String.intercalate "\n"
def unwords : List String -> String := String.intercalate " "

def List.splitOn (pred : α -> Bool): List α → List (List α)
  | [] => [[]]
  | x::xs =>
    match splitOn pred xs with
    | [] => [] -- unreachable
    | p::ps => if pred x then [] :: p :: ps else (x :: p) :: ps

def uncurry (f : α → β → γ) : (α × β → γ) := λ (a, b) => f a b