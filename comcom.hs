-- \x.x -> i
-- \x.M -> k M   if x is free in M
-- \x.QP -> S (\x.Q) (\x.P)

data Expression = Lam Char Expression | Var Char | App Expression Expression | S | K | I deriving (Show)

comp :: Expression -> Expression
comp (Lam x (Var y)) = if x == y then I else App K (Var y)
comp (Lam x (App y z)) = App (App S (comp (Lam x y))) (comp (Lam x z))
comp (Lam x (Lam y z)) = comp (Lam x (comp (Lam y z)))
comp (Lam x y) = App K y -- For lone combinators

prettyPrint :: Expression -> String
prettyPrint (App x y) = "(" ++ prettyPrint x ++ prettyPrint y ++ ")"
prettyPrint S = "S"
prettyPrint K = "K"
prettyPrint I = "I"