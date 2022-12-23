import Aoc
import Aoc.Lib.List

inductive BinOp :=
| Add
| Sub
| Mul
| Div
| NegSub
| InvDiv
deriving Inhabited

def BinOp.mk : String → BinOp
| "+" => Add
| "-" => Sub
| "*" => Mul
| "/" => Div
| _ => panic "invalid binop"

def BinOp.eval : BinOp → (Int → Int → Int)
| .Add => .add
| .Sub => .sub
| .Mul => .mul
| .Div => .div
| .NegSub => flip .sub
| .InvDiv => flip .div

inductive Monkey :=
| Num (n : Int)
| Op (op : BinOp) (lhs rhs : String)
deriving Inhabited

def Monkey.mk (s : String) : String × Monkey :=
  let (name, s) := s.splitOn ": " |>.first2!
  if let some n := s.toNat? then
    (name, Num n)
  else
    let (lhs, op, rhs) := words s |>.first3!
    (name, Op (BinOp.mk op) lhs rhs)

inductive Value :=
| Num (n : Int)
| Human (n : Int)
| Op (op : BinOp) (lhs rhs : Value)
deriving Inhabited

def Value.num? : Value → Option Int 
| Num n => n
| _ => none

def Value.op? : Value → Option (BinOp × Value × Value)
| Op op lhs rhs => (op, lhs, rhs)
| _ => none

abbrev Memo := List (String × Value)

partial def yell (monkeys : List (String × Monkey)) (name : String) : Value :=
  go name [] |>.lookup name |>.get!
  where
    go (name : String) (memo : Memo) : Memo := Id.run do
      if memo.lookup name |>.isSome then
        return memo
      match monkeys.lookup name |>.get! with
      | .Num n => (name, if name == "humn" then .Human n else .Num n) :: memo
      | .Op op lhs rhs => 
          let memo := memo |> go lhs |> go rhs
          let lhs := memo.lookup lhs |>.get!
          let rhs := memo.lookup rhs |>.get!
          (name, .Op op lhs rhs) :: memo

def Value.eval : Value → Int 
| Num n => n
| Human n => n
| Op op lhs rhs => op.eval lhs.eval rhs.eval

def Value.simpl : Value → Value
| Op op lhs rhs =>
  let lhs := lhs.simpl
  let rhs := rhs.simpl
  if let Num l := lhs then
    if let Num r := rhs then
      Num (op.eval l r)
    else
      Op op lhs rhs
  else
    Op op lhs rhs
| v => v

-- lhs `op` rhs = res
-- returns op' s.t. lhs := res `op'` rhs
def BinOp.calcLHS : BinOp → BinOp
| .Add => .Sub
| .Sub => .Add
| .Mul => .Div
| .Div => .Mul
| _ => panic "invalid op"

-- lhs `op` rhs = res
-- returns op' s.t. rhs := res `op'` lhs
def BinOp.calcRHS : BinOp → BinOp
| .Add => .Sub
| .Sub => .NegSub
| .Mul => .Div
| .Div => .InvDiv
| _ => panic "invalid op"

-- calculate value of .Human s.t. lhs == rhs
partial def Value.invert (lhs: Value) (rhs: Int) : Int := Id.run do
  if let .Human _ := lhs then
    return rhs
  let (op, xv, yv) := lhs.op?.get!
  if let .Num x := xv then
    return yv.invert <| op.calcRHS.eval rhs x
  if let .Num y := yv then
    return xv.invert <| op.calcLHS.eval rhs y
  panic "value tree inversion failed!"

def main : IO Unit := IO.interact $ λ input =>
  let monkeys := lines input |>.map Monkey.mk
  let root := yell monkeys "root" |>.simpl

  let part₁ := root.eval

  let (lhs, rhs) := match root with | .Op _ lhs rhs => (lhs, rhs) | _ => panic ""
  let (lhs, rhs) := if lhs.num?.isSome then (rhs, lhs) else (lhs, rhs)

  let part₂ := lhs.invert rhs.eval

  s!"{part₁}, {part₂}"