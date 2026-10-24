module Embedded.Hirane.Bit where

import qualified Prelude ()

newtype Bit = Bit { unBit :: forall x. x -> x -> x }

b0, b1 :: Bit
not :: Bit -> Bit
or, and, andBit, xor, eqBit :: Bit -> Bit -> Bit

b0 = Bit (\_ x -> x)
b1 = Bit (\x _ -> x)
not = \x -> unBit x b0 b1
or = \x y -> unBit x b1 y
and = \x y -> unBit x y b0
andBit = and
xor = \x y -> unBit x (unBit y b0 b1) y
eqBit = \x y -> unBit x y (unBit y b0 b1)