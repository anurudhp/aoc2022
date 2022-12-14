import Aoc
import Aoc.Lib.List

inductive Command :=
| Noop
| AddX (v : Int)

def parseCommand (s : String) : Command :=
  if s.startsWith "addx" then
    Command.AddX (s.drop 5 |>.toInt!)
  else
    Command.Noop

abbrev Hist := List Int

def init : Hist := [1, 1]

def Command.run (xs : Hist) : Command →  Hist
| Noop   => x :: xs
| AddX v => (x + v) :: x :: xs
  where
    x := xs.head!

def main : IO Unit := IO.interact $ λ input =>
  let hist := lines input 
   |>.map parseCommand
   |>.foldl Command.run init
   |>.reverse
   |>.toArray
  
  let strength := [20, 60, 100, 140, 180, 220].map (λ i => i * hist.get! i) |>.foldl Int.add 0

  let drawing := Id.run do
    let sz := 6 * 40
    let mut screen := List.replicate sz '.'
    for i in [0:hist.size - 1] do
      let val := if (hist.get! (i + 1) - i.mod 40).natAbs <= 1 then '#' else '.'
      screen := screen.set (i.mod sz) val
    return screen.toChunks 40 |>.map (String.mk) |> unlines

  s!"{strength}\n{drawing}"