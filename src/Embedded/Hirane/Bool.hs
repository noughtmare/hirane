module Embedded.Hirane.Bool where

import qualified Prelude ()

import Embedded.Hirane.Bit

newtype Bool = Bool { unBool' :: Bit }

unBool :: Bool -> x -> x -> x
unBool = \x y z -> unBit (unBool' x) y z

false, true :: Bool
false = Bool b0
true = Bool b1

not :: Bool -> Bool
not = \x -> unBool x false true

or, and :: Bool -> Bool -> Bool
or = \x y -> unBool x true y
and = \x y -> unBool x y false

toBit :: Bool -> Bit
toBit = \x -> unBool x b1 b0
fromBit :: Bit -> Bool
fromBit = \x -> unBit x true false