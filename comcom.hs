-- \x.x  -> i
-- \x.M  -> k M   if x is free in M
-- \x.QP -> S (\x.Q) (\x.P)

data Expression = Lam Char Expression | Var Char | App Expression Expression | S | K | I deriving (Show)

compile :: Expression -> Expression
compile (Lam x (Var y)) = if x == y then I else App K (Var y)
compile (Lam x (App y z)) = App (App S (compile (Lam x y))) (compile (Lam x z))
compile (Lam x (Lam y z)) = compile (Lam x (compile (Lam y z)))
compile (Lam x y) = App K y -- For lone combinators

prettyPrint :: Expression -> String
prettyPrint (App x (App y z)) = prettyPrint x ++ "(" ++ prettyPrint (App y z) ++ ")"
prettyPrint (App x y) = prettyPrint x ++ prettyPrint y
prettyPrint S = "S"
prettyPrint K = "K"
prettyPrint I = "I"

-- prettyPrint (comp (Lam 'x' (Lam 'y' (App (Var 'x') (Var 'y')))))
-- "S(S(KS)(S(KK)I))(KI)"