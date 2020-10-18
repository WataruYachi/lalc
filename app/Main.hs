module Main where

import Control.Applicative
import Control.Monad
import Data.Char

import Lib
import Parser
import Calc

data Ident = Ident String

instance Show Ident where
    show (Ident a) = a

infixl 9 :$
infixl 7 :\

data LExpr
    = Var Ident
    | Ident :\ LExpr
    | LExpr :$ LExpr

instance Show LExpr where
    show (Var a) = show a
    show (id :\ exp) = "λ" ++ show id ++ "." ++ (show exp)
    show (exp1 :$ exp2) = (show exp1) ++ " " ++ (show exp2)

main :: IO ()
main = someFunc
