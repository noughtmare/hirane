module Language.Hirane.AST where

data Exp = Var String | Lam String Exp | App Exp Exp

