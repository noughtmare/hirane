module Embedded.Hirane.Int where

import qualified Prelude

import Embedded.Hirane.Fix
import Embedded.Hirane.Bit
import Embedded.Hirane.Ord
import Embedded.Hirane.Bool (unBool)
import Embedded.Hirane.Nat (Nat, unNat)
import qualified Embedded.Hirane.Nat as Nat
import Embedded.Hirane.Product

import qualified Embedded.Hirane.HS as Debug

newtype Int = Int { unInt :: forall x. (Nat -> x) -> (Nat -> x) -> x -> x }

zero :: Int
zero = Int (\_ _ z -> z)

neg1 :: Int
neg1 = Int (\n _ _ -> n (Nat.pushBit b1 Nat.zero))

pos1 :: Int
pos1 = Int (\_ p _ -> p (Nat.pushBit b1 Nat.zero))

neg :: Nat -> Int
neg = \x -> unBool (Nat.ifzero x) zero (Int (\n _ _ -> n x))

pos :: Nat -> Int
pos = \x -> unBool (Nat.ifzero x) zero (Int (\_ p _ -> p x))

abs :: Int -> Nat
abs = \x -> unInt x (\z -> z) (\z -> z) Nat.zero

btimes2, btimes2plus1 :: Nat -> Nat
btimes2 = Nat.pushBit b0
btimes2plus1 = Nat.pushBit b1

pushBit :: Bit -> Int -> Int
pushBit b x = unInt x
  (\x -> Int (\n _ _ -> n (Nat.pushBit b x)))
  (\x -> Int (\_ p _ -> p (Nat.pushBit b x)))
  (unBit b (Int (\_ p _ -> p (Nat.pushBit b1 Nat.zero))) zero)

-- assumes x is at least 1
subBit :: Nat -> Bit -> Nat
subBit x b = unBit b (sub' x (Nat.pushBit b1 Nat.zero)) x

-- 01
-- 10
-- --
-- 01 (negative)

-- 101
-- 010
-- 011 (pos)

-- 10
-- 11
-- 01

sub :: Nat -> Nat -> Int
sub = \x y -> unOrdering (Nat.cmp x y)
  (neg (sub' y x))
  zero
  (pos (sub' x y))

sub' :: Nat -> Nat -> Nat
sub' = fix (\add c x y ->
  let
    -- x | y | c || c' | x'
    -- 0 | 0 | 0 || 0 | 0
    -- 0 | 0 | 1 || 1 | 1
    -- 0 | 1 | 0 || 1 | 1
    -- 0 | 1 | 1 || 1 | 0
    -- 1 | 0 | 0 || 0 | 1
    -- 1 | 0 | 1 || 0 | 0
    -- 1 | 1 | 0 || 0 | 0
    -- 1 | 1 | 1 || 1 | 1
    nextCarry = \bx by -> or (and (not bx) (or by c)) (and (and bx by) c)
    currentBit = \bx by -> xor bx (xor by c)
  in
  unNat x
    (\x' -> unNat y
      (\y' -> Nat.pushBit (currentBit b0 b0) (add (nextCarry b0 b0) x' y'))
      (\y' -> Nat.pushBit (currentBit b0 b1) (add (nextCarry b0 b1) x' y'))
      (subBit x c))
    (\x' -> unNat y
      (\y' -> Nat.pushBit (currentBit b1 b0) (add (nextCarry b1 b0) x' y'))
      (\y' -> Nat.pushBit (currentBit b1 b1) (add (nextCarry b1 b1) x' y'))
      (subBit x c))
    (Nat.addBit c y)
  ) b0

div :: Nat -> Nat -> Prod Nat Nat
div = fix (\bdiv n d ->
  let
    r :: Prod Nat Nat
    r = unNat n
      (\n' -> unProd (bdiv n' d) (\q r ->
        let
          r' = Nat.pushBit b0 r
        in
          unInt (sub r' d) (\_ -> prod (Nat.pushBit b0 q) r') (\r'' -> prod (Nat.pushBit b1 q) r'') (prod (Nat.pushBit b1 q) Nat.zero)))
      (\n' -> unProd (bdiv n' d) (\q r ->
        let
          r' = Nat.pushBit b1 r
        in
          unInt (sub r' d) (\_ -> prod (Nat.pushBit b0 q) r') (\r'' -> prod (Nat.pushBit b1 q) r'') (prod (Nat.pushBit b1 q) Nat.zero)))
      (prod Nat.zero Nat.zero)
  in
    unOrdering (Nat.cmp n d) (prod Nat.zero n) r r)

-- >>> let (Debug.Iso to from) = Debug.embedNat in from (abs (sub (to 3) (to 4)))
-- 1

-- >>> let (Debug.Iso to from) = Debug.embedNat in from (abs (sub (to 1) (to 2)))
-- 0

-- >>> let (Debug.Iso to from) = Debug.embedNat in from (abs (sub (to 1) (to 3)))
-- 0

-- >>> let (Debug.Iso to from) = Debug.embedNat in from (abs (sub (to 2) (to 3)))
-- 1

-- >>> let (Debug.Iso to from) = Debug.embedNat in unInt (sub (to 1) (to 3)) (\x -> ("negative", from x)) (\x -> ("positive", from x)) ("zero", 0)
-- ("zero",0)

-- >>> let (Debug.Iso to from) = Debug.embedNat in unProd (div (to 3) (to 2)) (\q r -> (from q, from r))
-- (1,1)

-- cmp :: Nat -> Nat -> Ordering
-- cmp = \x y -> unNat x
--   (\x -> unNat y
--     (\y -> cmp x y)
--     (\y -> Ord.seq (cmp x y) Ord.lt)
--     (cmp x zero))
--   (\x -> unNat y
--     (\y -> Ord.seq (cmp x y) Ord.gt)
--     (\y -> cmp x y)
--     Ord.gt)
--   (unNat y (\y -> cmp zero y) (\_ -> Ord.lt) Ord.eq)
--
-- eq :: Nat -> Nat -> Bool
-- eq = \x y -> Ord.unOrdering (cmp x y) false true false
--
-- zero :: Nat
-- zero = Nat (\_ _ z -> z)
--
-- suc :: Nat -> Nat
-- suc = \n -> add n (pushBit b1 zero)
--
-- -- Note: little endian
-- pushBit :: Bit -> Nat -> Nat
-- pushBit = \b x -> Nat (\e o _ -> unBit b o e x)
--
-- add :: Nat -> Nat -> Nat
-- add = fix (\add c x y ->
--   let
--     nextCarry bx by = or (and c bx) (or (and c by) (and bx by))
--     currentBit bx by = xor c (xor bx by)
--   in
--   unNat x
--     (\x' -> unNat y
--       (\y' -> pushBit (currentBit b0 b0) (add (nextCarry b0 b0) x' y'))
--       (\y' -> pushBit (currentBit b0 b1) (add (nextCarry b0 b1) x' y'))
--       x)
--     (\x' -> unNat y
--       (\y' -> pushBit (currentBit b1 b0) (add (nextCarry b1 b0) x' y'))
--       (\y' -> pushBit (currentBit b1 b1) (add (nextCarry b1 b1) x' y'))
--       x)
--     y
--   ) b0
--
-- pred :: Nat -> Maybe Nat
-- pred = \n -> unNat n
--   (\x -> mapMaybe (pushBit b1) (pred x))
--   (\x ->
-- just (pushBit b0 x))
--   nothing
