data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

addNat :: Nat -> Nat -> Nat
addNat x y = int2nat (nat2int x + nat2int y)

addNatre :: Nat -> Nat -> Nat
addNatre Zero y = y
addNatre (Succ x) y = Succ (addNatre x y)

data Ex
  = Val Int
  | Madd Ex Ex
  | Mmul Ex Ex

size :: Ex -> Int
size (Val n) = 1
size (Madd x y) = size x + size y
size (Mmul x y) = size x + size y

eval :: Ex -> Int
eval (Val n) = n
eval (Madd x y) = eval x + eval y
eval (Mmul x y) = eval x * eval y

multi :: Nat -> Nat -> Nat
multi Zero y = Zero
multi (Succ Zero) y = y
multi (Succ x) y = addNatre y (multi x y)

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)

data Expr = Value Int | Add Expr Expr | Mul Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
folde f g l (Value x) = f x
folde f g l (Add x y) = g (folde f g l x) (folde f g l y)
folde f g l (Mul x y) = l (folde f g l x) (folde f g l y)

myeval :: Expr -> Int
myeval = folde id (+) (*)

lee :: Expr
lee = Add (Value 2) (Mul (Value 3) (Value 4))
