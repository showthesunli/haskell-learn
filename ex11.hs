data Op = Add | Sub | Mul | Div
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"


apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

data Expression = Val Int | App Op Expression Expression
instance Show Expression where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
        where
            brak (Val n) = show n
            brak e = "("++show e++")"

eval :: Expression -> [Int]
eval (Val n) = [n| n > 0]
eval (App o l r) = [apply o l' r' | l'<- eval l, r'<- eval r, valid o l' r']

insertInto :: a -> [a] -> [[a]]
insertInto x [] = [[x]]
insertInto x (y:ys) = (x:y:ys): map (y:) (insertInto x ys)

choices :: [a] -> [[a]]
choices [] = [[]]
choices (x:xs) = choices xs ++ concat [insertInto x ys| ys <- choices xs]

values :: Expression -> [Int]
values (Val x) = [x]
values (App _ x y) = values x ++ values y

solution :: Expression -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split [x,y] = [([x], [y])]
split (x:xs) = ([x], xs) : map (\(y, z) -> (x:y, z)) (split xs)

exprs :: [Int] -> [Expression]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e| (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combineop l r ]

combineop :: Expression -> Expression -> [Expression]
combineop l r = [App o l r | o <- [Add, Sub, Mul, Div]]

solutions :: [Int] -> Int -> [Expression]
solutions ns n = [e | vs <- choices ns, e <- exprs vs, eval e == [n]]

type Result = (Expression, Int)
