import Aoc
import Aoc.Lib.List
import Aoc.Lib.Mergesort

inductive Packet :=
| Num (x : Nat)
| Nest (ps : List Packet)
deriving BEq, Inhabited

def Packet.get : Packet → List Packet
| p@(Num _) => [p]
| Nest ps   => ps
  
partial def Packet.cmp : Packet → Packet → Ordering
| Num u, Num v => compare u v
| p, q => Id.run do
  let p := p.get
  let q := q.get
  for (u, v) in p.zip q do
    let res := u.cmp v
    if res != .eq then return res
  compare p.length q.length

instance : Ord Packet where
  compare := Packet.cmp

inductive StackVal :=
| Open
| Num (n : Nat)
| Pack (p : Packet)
deriving BEq

def StackVal.getNum? : StackVal → Option Nat
| Num n => n
| _ => none

def StackVal.getPacket? : StackVal → Option Packet
| Pack p => p
| Num n => Packet.Num n
| _ => none

def parsePacket (s : String) : Option Packet := go [] s.data where
  go (stk : List StackVal) : List Char → Option Packet
  | [] => do (← stk.head?).getPacket?

  | '[' :: cs => go (.Open :: stk) cs

  | ',' :: cs => do
    let mut stk := stk
    if let some u := stk.head? then
      if let some u := u.getNum? then
        stk ← stk.tail?
        stk := .Pack (.Num u) :: stk
    go stk cs

  | ']' :: cs => do
    let mut stk := stk
    let mut ps := []

    while true do
      let u ← stk.head?
      stk ← stk.tail?

      if u == .Open then break
      let u ← u.getPacket?
      ps := u :: ps

    stk := .Pack (.Nest ps) :: stk
    go stk cs

  | d :: cs => do
    let mut v := d.toNat - '0'.toNat
    let mut stk := stk
    if let some u := stk.head? then
      if let some u := u.getNum? then
        v := 10 * u + v 
        stk ← stk.tail?
    go (.Num v :: stk) cs
  

@[reducible]
def PackPair := Packet × Packet

def PackPair.ble : PackPair → Bool
| (p, q) => compare p q |>.isLE

def parsePair? : List String → Option PackPair
| [p, q] => do
  let p ← parsePacket p
  let q ← parsePacket q
  return (p, q)
| _ => none

-- [1..], theoretically :)
def ixs := List.range 10000 |>.drop 1

def main : IO Unit := IO.interact $ λ input =>
  let pairs := lines input
    |>.splitOn String.isEmpty
    |>.map parsePair?
    |>.catOptions

  -- part 1
  let correct := pairs
    |>.zip ixs
    |>.filter (Prod.fst :> PackPair.ble)
    |>.map Prod.snd
    |>.foldl .add 0

  -- part 2
  let divp2 := parsePacket "[[2]]" |>.get!
  let divp6 := parsePacket "[[6]]" |>.get!
  
  let packets := pairs
    |>.map (λ (p, q) => [p, q])
    |>.join
    |>.cons divp2
    |>.cons divp6
    |>.mergeSort
    |>.zip ixs
  
  let ix2 := packets.lookup divp2 |>.get!
  let ix6 := packets.lookup divp6 |>.get!

  s!"{correct}, {ix2 * ix6}"