module Calc where

import Parser

data Expr
    = Val Term
    | Sum Term Expr
    | Sub Term Expr
    deriving (Show)

data Term
    = Term Factor
    | Pro Factor Term
    | Div Factor Term
    deriving (Show)

data Factor
    = Factor Expr
    | Num Int
    | Pow Int Expr
    deriving (Show)

evalE :: Expr -> Int
evalE (Val t) = evalT t
evalE (Sum t e) = evalT t + evalE e
evalE (Sub t e) = evalT t - evalE e

evalT :: Term -> Int
evalT (Term f) = evalF f
evalT (Pro f t) = evalF f * evalT t
evalT (Div f t) = evalF f `div` evalT t

evalF :: Factor -> Int
evalF (Factor f) = evalE f
evalF (Num n) = n
evalF (Pow n e) = n ^ evalE e