import Aoc
import Aoc.Lib.List

abbrev BinOp := Nat → Nat → Nat

def BinOp.mk : String → BinOp
| "+" => .add
| "-" => .sub
| "*" => .mul
| "/" => .div
| _ => panic "invalid binop"

inductive Monkey :=
| Val (n : Nat)
| Op (op : BinOp) (lhs rhs : String)
deriving Inhabited

def Monkey.mk (s : String) : String × Monkey :=
  let (name, s) := s.splitOn ": " |>.first2!
  if let some n := s.toNat? then
    (name, Val n)
  else
    let (lhs, op, rhs) := words s |>.first3!
    (name, Op (BinOp.mk op) lhs rhs)

abbrev Memo := List (String × Nat)

partial def yell (monkeys : List (String × Monkey)) (name : String) : Nat :=
  go name [] |>.lookup name |>.get!
  where
    go (name : String) (memo : Memo) : Memo := Id.run do
      if memo.lookup name |>.isSome then
        return memo
      match monkeys.lookup name |>.get! with
      | .Val n => (name, n) :: memo
      | .Op op lhs rhs => 
          let memo := memo |> go lhs |> go rhs
          let lhs := memo.lookup lhs |>.get!
          let rhs := memo.lookup rhs |>.get!
          (name, op lhs rhs) :: memo

def main : IO Unit := IO.interact $ λ input =>
  let monkeys := lines input |>.map Monkey.mk
  let root := yell monkeys "root"

  s!"{root}"