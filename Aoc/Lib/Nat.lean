def Nat.lcm (u v : Nat) : Nat := u * v / (v.gcd u)