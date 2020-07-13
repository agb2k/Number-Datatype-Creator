data Nat = Zero | Succ Nat
           deriving (Eq, Ord, Show, Read)


addNat :: Nat -> Nat -> Nat
addNat Zero     n = n
addNat (Succ m) n = Succ (addNat m n) 

multNat :: Nat -> Nat -> Nat
multNat Zero n     = Zero
multNat (Succ m) n = addNat (multNat m n) n

powNat :: Nat -> Nat -> Nat
powNat m (Succ Zero) = m
powNat m (Succ n) = multNat (powNat m n) m

facNat :: Nat -> Nat
facNat (Succ Zero) = Succ Zero
facNat (Succ m) = multNat (Succ m) (facNat m)
