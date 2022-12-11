import Aoc
import Aoc.Lib.List
import Aoc.Lib.Array
import Aoc.Lib.Mergesort

namespace Day11

inductive Monkey := 
| M (op : Nat → Nat) (tst tru fls : Nat) (items : List Nat) (cnt : Nat)
deriving Inhabited

def Monkey.inspect : Monkey → Nat → Nat
| M op _ _ _ _ _ => op

def Monkey.items : Monkey → List Nat
| M _ _ _ _ items _ => items.reverse

def Monkey.inspectCount : Monkey → Nat
| M _ _ _ _ _ cnt => cnt

def Monkey.test : Monkey → Nat → Nat
| M _ tst tru fls _ _, it => if it.mod tst == 0 then tru else fls

def Monkey.add : Nat → Monkey → Monkey
| it, M op tst tru fls items cnt => M op tst tru fls (it :: items) cnt

def Monkey.clear : Monkey → Monkey
| M op tst tru fls items cnt => M op tst tru fls [] (cnt + items.length)

def parseMonkey (ws : List String) : Monkey :=
  -- "Monkey <ix>:"
  let ix := ws.get! 0
    |>.splitOn " "
    |>.get! 1
    |>.dropRight 1
    |>.toNat!

  -- "Starting items: <a_1, a_2, ..., a_n>"
  let items := ws.get! 1
    |>.splitOn ":"
    |>.get! 1
    |>.splitOn ","
    |>.map String.trim
    |>.map String.toNat!
    |>.reverse

  -- "Operation: new = old <+|*> <old|num>"
  let expr := ws.get! 2
    |>.splitOn " "
    |>.drop 4
  let oper := if expr.get! 0 == "+" then Nat.add else Nat.mul
  let op := match expr.get! 1 with
    | "old" => λ x => oper x x
    | y     => λ x => oper x y.toNat!

  let kth (i k : Nat) :=
    ws.get! i -- line i
    |>.splitOn " "
    |>.get! k -- word k
    |>.toNat!

  -- "Test: divisible by <N>"
  let tst := kth 3 3
  -- "If true: throw to monkey <N>"
  let tru := kth 4 5
  -- "If false: throw to monkey <N>"
  let fls := kth 5 5

  Monkey.M op tst tru fls items 0

def runRound (monkeys : Array Monkey) : Array Monkey := Id.run do
  let n := monkeys.size
  let mut ms := monkeys
  for ix in [:n] do
    let m := ms.get! ix
    for it in m.items do
      let it := m.inspect it |>.div 3
      let targ := it |> m.test
      ms := ms.upd! targ (.add it)
    ms := ms.upd! ix .clear
  ms

def main (inp : String) : String :=
  let monkeys := lines inp
    |>.map .trim
    |>.toChunks 7
    |>.map parseMonkey
    |>.toArray

  let monkeybusiness := Id.run do
    let mut ms := monkeys
    for _ in [:20] do
      ms := runRound ms
    ms.map Monkey.inspectCount
      |>.toList
      |>.mergeSort
      |>.reverse
      |>.take 2
      |>.foldl Nat.mul 1

  s!"{monkeybusiness}"