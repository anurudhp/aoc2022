def sum [Add α] [Inhabited α] (xs : List α) : α := Id.run do
  let mut s := Inhabited.default
  for x in xs do
    s := s + x
  return s

def maximum (xs : List Int) : Int := Id.run do
  let mut mx := 0
  for x in xs do
    mx := max mx x
  return mx