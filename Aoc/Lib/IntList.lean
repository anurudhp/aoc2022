def sum (xs : List Int) : Int := Id.run do
  let mut s := 0
  for x in xs do
    s := s + x
  return s

def maximum (xs : List Int) : Int := Id.run do
  let mut mx := 0
  for x in xs do
    mx := max mx x
  return mx