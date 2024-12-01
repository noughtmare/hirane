module Embedded.Hirane.DList where

import qualified Prelude ()

import qualified Embedded.Hirane.List as List
import Embedded.Hirane.List (List)

newtype DList a = DList { unDList :: List a -> List a }

nil :: DList a
nil = DList (\x -> x)

cons :: a -> DList a -> DList a
cons x xs = DList (\k -> List.cons x (unDList xs k))

snoc :: DList a -> a -> DList a
snoc xs x = DList (\k -> unDList xs (List.cons x k))

append :: DList a -> DList a -> DList a
append = \xs ys -> DList (\k -> unDList xs (unDList ys k))

fromList :: List a -> DList a
fromList = \xs -> DList (List.append xs)

toList :: DList a -> List a
toList = \xs -> unDList xs List.nil