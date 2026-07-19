-- A compiler from lambda calculus to combinatory logic using the S, K, and I combinators.

-- In combinatory logic :-
-- S f g x = f x (g x)
-- K x y = x
-- I x = x

-- The rules for the translation are as follows :-
-- \x.x  -> i
-- \x.M  -> k M   if x is not free in M
-- \x.QP -> S (\x.Q) (\x.P)



data Expression = Lam Char Expression | Var Char | App Expression Expression | S | K | I deriving (Show)

compile :: Expression -> Expression
compile (Lam x (Var y)) = if x == y then I else App K (Var y)
compile (Lam x (App y z)) = App (App S (compile (Lam x y))) (compile (Lam x z))
compile (Lam x (Lam y z)) = compile (Lam x (compile (Lam y z)))
compile (Lam x y) = App K y -- For lone combinators

simplify :: Expression -> Expression
simplify (App x y) = case simplify x of
  I -> simplify y
  App K a -> simplify a
  App (App S a) b -> simplify (App (App a y) (App b y))

  App S K -> I
  App S (App K a) -> case simplify y of
    I -> simplify a
    App K b -> simplify (App K (App a b))
    b -> App (App S (App K a)) b
  a -> App a (simplify y)

simplify (Lam x body) = Lam x (simplify body)
simplify expression = expression

prettyPrint :: Expression -> String
prettyPrint (App x (App y z)) = prettyPrint x ++ "(" ++ prettyPrint (App y z) ++ ")"
prettyPrint (App x y) = prettyPrint x ++ prettyPrint y
prettyPrint S = "S"
prettyPrint K = "K"
prettyPrint I = "I"
prettyPrint e = error "Not able to express expression in combinators. Are there any free variables?"

run :: Expression -> String
run expr = prettyPrint (simplify (compile expr))

-- prettyPrint (comp (Lam 'x' (Lam 'y' (App (Var 'x') (Var 'y')))))
-- "S(S(KS)(S(KK)I))(KI)"
