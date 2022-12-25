import Aoc

abbrev SNAFU := String

def Char.conv : Char → Int
| '2' =>  2
| '1' =>  1
| '0' =>  0
| '-' => -1
| '=' => -2
| _ => panic "unreachable"

def Int.conv : Int → Char
|  2 => '2'
|  1 => '1'
|  0 => '0'
| -1 => '-'
| -2 => '='
| _ => panic "unreachable"

def SNAFU.toInt! (s : SNAFU) : Int := s.foldl (· * 5 + ·.conv) 0

def Int.toSNAFU! (n : Int) : SNAFU := Id.run do
  let mut n := n
  let mut res := [] 
  while n > 0 do
    let mut d := n % 5
    n := n / 5
    if d >= 3 then
      d := d - 5
      n := n + 1
    res := d.conv :: res
  ⟨res⟩

def main : IO Unit := IO.interact
  (· |> lines |>.map SNAFU.toInt! |>.foldl Int.add 0 |>.toSNAFU!)