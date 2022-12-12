import Aoc.Lib.IO
-- Helpers

notation:1000 f ":>" g => g ∘ f

def lines (s : String) : List String := String.splitOn s "\n" |>.reverse |>.dropWhile String.isEmpty |>.reverse
def words (s : String) : List String := s.split Char.isWhitespace |>.filter (not ∘ String.isEmpty)

def unlines : List String -> String := String.intercalate "\n"

def uncurry (f : α → β → γ) : (α × β → γ) := λ (a, b) => f a b