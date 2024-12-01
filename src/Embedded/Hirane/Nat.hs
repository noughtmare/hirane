module Embedded.Hirane.Nat where

import qualified Prelude

import Embedded.Hirane.Fix
import Embedded.Hirane.Bit
import Embedded.Hirane.Bool (Bool, true, false)
import Embedded.Hirane.Maybe
import qualified Embedded.Hirane.Ord as Ord
import Embedded.Hirane.Ord (Ordering)

newtype Nat = Nat { unNat :: forall x. (Nat -> x) -> (Nat -> x) -> x -> x }

cmp :: Nat -> Nat -> Ordering
cmp = \x y -> unNat x
  (\x -> unNat y
    (\y -> cmp x y)
    (\y -> Ord.seq (cmp x y) Ord.lt)
    (cmp x zero))
  (\x -> unNat y
    (\y -> Ord.seq (cmp x y) Ord.gt)
    (\y -> cmp x y)
    Ord.gt)
  (unNat y (\y -> cmp zero y) (\_ -> Ord.lt) Ord.eq)

eq :: Nat -> Nat -> Bool
eq = \x y -> Ord.unOrdering (cmp x y) false true false

zero :: Nat
zero = Nat (\_ _ z -> z)

suc :: Nat -> Nat
suc = add (pushBit b1 zero)

-- Note: little endian
pushBit :: Bit -> Nat -> Nat
pushBit = \b x -> Nat (\e o _ -> unBit b o e x)

debugNat :: Nat -> Prelude.Int
debugNat x = unNat x (\x -> debugNat x Prelude.* 2) (\x -> debugNat x Prelude.* 2 Prelude.+ 1) 0

addBit :: Bit -> Nat -> Nat
addBit = \b x -> unBit b (add (pushBit b1 zero) x) x

add :: Nat -> Nat -> Nat
add = fix (\add c x y ->
  let
    nextCarry bx by = or (and c bx) (or (and c by) (and bx by))
    currentBit bx by = xor c (xor bx by)
  in
  unNat x
    (\x' -> unNat y
      (\y' -> pushBit (currentBit b0 b0) (add (nextCarry b0 b0) x' y'))
      (\y' -> pushBit (currentBit b0 b1) (add (nextCarry b0 b1) x' y'))
      (addBit c x))
    (\x' -> unNat y
      (\y' -> pushBit (currentBit b1 b0) (add (nextCarry b1 b0) x' y'))
      (\y' -> pushBit (currentBit b1 b1) (add (nextCarry b1 b1) x' y'))
      (addBit c x))
    (addBit c y)
  ) b0

ifzero :: Nat -> Bool
ifzero = \n -> unNat n
  (\x -> ifzero x)
  (\_ -> false)
  true

mul :: Nat -> Nat -> Nat
mul = fix (\mul x y -> unNat x
  (\x -> mul x (pushBit b0 y))
  (\x -> add y (mul x (pushBit b0 y)))
  zero)

-- >>> debugNat (mul (suc (suc (suc zero))) (suc (suc (suc zero))))
-- 9

-- >>> debugNat (suc (suc (suc zero)))
-- 3

pred :: Nat -> Maybe Nat
pred = \n -> unNat n
  (\x -> mapMaybe (pushBit b1) (pred x))
  (\x -> just (pushBit b0 x))
  nothing
