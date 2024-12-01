module Embedded.Hirane.HS where

import Embedded.Hirane.Nat as Hirane
import Embedded.Hirane.List as Hirane
import Data.Bits
import Data.Char

data Iso a b = Iso
  { to :: a -> b
  , from :: b -> a
  }

embedNat :: Iso Int Hirane.Nat
embedNat = Iso
  { to = \x -> Hirane.Nat (\e o z ->
  if x == zeroBits then
    z
  else if testBit x 0 then
    o (to embedNat (shiftR x 1))
  else
    e (to embedNat (shiftR x 1)))
  , from = \x -> Hirane.unNat x
  (\x' -> shiftL (from embedNat x') 1)
  (\x' -> shiftL (from embedNat x') 1 .|. bit 0)
  zeroBits
  }

embedString :: Iso String (Hirane.List Hirane.Nat)
embedString = Iso
  { to = \xs -> case xs of
  [] -> Hirane.nil
  (x:xs) -> Hirane.cons (to embedNat (ord x)) (to embedString xs)
  , from = \xs ->
  Hirane.unList xs
    (\c xs' -> chr (from embedNat c) : from embedString xs')
    ""
  }
