import Aoc
import Aoc.Lib.List

@[reducible] def Dir := List String

inductive Folder :=
| Folder (files : List (String × Nat)) (dirs : List Dir)
deriving Inhabited

inductive Command :=
| cd (dir : String)
| ls (listing : Folder)

def parseCommand (s : String) : Command := 
  let ws := words s
  if ws.head! == "cd" then
    Command.cd (ws.get! 1)
  else 
    let ws := ws |>.drop 1 |>.toChunks 2
    let dirs := ws
                |>.filter (λ l => l.head! == "dir")
                |>.map (λ l => [l.get! 1])
    let files := ws
                |>.filter (λ l => l.head! != "dir")
                |>.map (λ l => (l.get! 1, l.head!.toNat!))
    Command.ls $ Folder.Folder files dirs 

inductive Info :=
| State (cwd : Dir) (listings : List (Dir × Folder))

def Info.mk : Info := State [] []

def Info.extract : Info → List (Dir × Folder)
| State _ ls => ls

def Info.cd : Info → (List String → List String) → Info 
| State cwd ls, f => State (f cwd) ls

def Command.run : Info → Command → Info 
| s, (cd dir) =>
  match dir with
  | "/" => s.cd (λ _ => [])
  | ".." => s.cd (List.drop 1)
  | name => s.cd (λ cwd => name :: cwd)
| (Info.State cwd listings), (ls (Folder.Folder files dirs)) => 
  let dirs := dirs.map (λ d => d ++ cwd)
  Info.State cwd ((cwd, Folder.Folder files dirs) :: listings)

inductive FSTree :=
| Node (name : String) (size : Nat) (children : List FSTree)
| Leaf (name : String) (size : Nat)
deriving Inhabited

def FSTree.size : FSTree → Nat
| Node _ sz _ => sz
| Leaf _ sz   => sz

partial def buildTree (ls : List (Dir × Folder)) (dir : Dir) : FSTree := 
  let (Folder.Folder files dirs) := ls.lookup dir |>.get!
  let files := files.map (uncurry FSTree.Leaf)
  let dirs := dirs.map (buildTree ls)
  let total := files ++ dirs |>.map FSTree.size |>.foldl Nat.add 0
  FSTree.Node (dir.head? |>.getD "") total dirs 

partial def FSTree.part1 : FSTree → Nat
| Node _ sz sub => (if sz <= 100000 then sz else 0) + (sub.map part1).foldl Nat.add 0
| _ => 0

def inf : Nat := 70000001

partial def FSTree.part2 (req : Nat) : FSTree → Nat
| Node _ sz sub => 
  let cur := (if sz >= req then sz else inf)
  sub.map (FSTree.part2 req) |>.foldl min cur
| _ => inf

def main : IO Unit := IO.interact $ λ input =>
  let fs := input
    |>.splitOn "$"
    |>.drop 1
    |>.map String.trim
    |>.map parseCommand
    |>.foldl Command.run Info.mk
    |>.extract
    |> buildTree $ [] 
  s!"{fs.part1}, {fs.part2 (fs.size - 40000000)}"