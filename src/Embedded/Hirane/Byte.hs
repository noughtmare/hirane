module Embedded.Hirane.Byte where

import qualified Prelude ()

import qualified Embedded.Hirane.Bit as Bit
import Embedded.Hirane.Bit (Bit, b0, b1, eqBit, andBit)
import Embedded.Hirane.Maybe
import Embedded.Hirane.Nat
import Embedded.Hirane.Bool

newtype Eight = Eight { unEight :: forall x. x -> x -> x -> x -> x -> x -> x -> x -> x }

e0, e1, e2, e3, e4, e5, e6, e7 :: Eight
e0 = Eight (\_ _ _ _ _ _ _ x -> x)
e1 = Eight (\_ _ _ _ _ _ x _ -> x)
e2 = Eight (\_ _ _ _ _ x _ _ -> x)
e3 = Eight (\_ _ _ _ x _ _ _ -> x)
e4 = Eight (\_ _ _ x _ _ _ _ -> x)
e5 = Eight (\_ _ x _ _ _ _ _ -> x)
e6 = Eight (\_ x _ _ _ _ _ _ -> x)
e7 = Eight (\x _ _ _ _ _ _ _ -> x)

add :: Eight -> Eight -> Maybe Eight
add = \x y ->
  unEight x
    (unEight y nothing nothing nothing nothing nothing nothing nothing (just e7))
    (unEight y nothing nothing nothing nothing nothing nothing (just e7) (just e6))
    (unEight y nothing nothing nothing nothing nothing (just e7) (just e6) (just e5))
    (unEight y nothing nothing nothing nothing (just e7) (just e6) (just e5) (just e4))
    (unEight y nothing nothing nothing (just e7) (just e6) (just e5) (just e4) (just e3))
    (unEight y nothing nothing (just e7) (just e6) (just e5) (just e4) (just e3) (just e2))
    (unEight y nothing (just e7) (just e6) (just e5) (just e4) (just e3) (just e2) (just e1))
    (unEight y (just e7) (just e6) (just e5) (just e4) (just e3) (just e2) (just e1) (just e0))

fromNat :: Nat -> Maybe Eight
fromNat = \n ->
  unMaybe (pred n)
    (\n -> unMaybe (pred n)
      (\n -> unMaybe (pred n)
        (\n -> unMaybe (pred n)
          (\n -> unMaybe (pred n)
            (\n -> unMaybe (pred n)
              (\n -> unMaybe (pred n)
                (\n -> unMaybe (pred n)
                  (\_ -> nothing)
                  (just e7))
                (just e6))
              (just e5))
            (just e4))
          (just e3))
        (just e2))
      (just e1))
    (just e0)

toNat :: Eight -> Nat
toNat e = unEight e
  (pushBit b1 (pushBit b1 (pushBit b1 zero)))
  (pushBit b0 (pushBit b1 (pushBit b1 zero)))
  (pushBit b1 (pushBit b0 (pushBit b1 zero)))
  (pushBit b0 (pushBit b0 (pushBit b1 zero)))
  (pushBit b1 (pushBit b1 zero))
  (pushBit b0 (pushBit b1 zero))
  (pushBit b1 zero)
  zero

newtype Byte = Byte { unByte :: Eight -> Bit }

by00, byFF :: Byte
by00 = Byte (\_ -> b0)
byFF = Byte (\_ -> b1)

and, or, xor :: Byte -> Byte -> Byte
and = \x y -> Byte (\i -> Bit.and (unByte x i) (unByte y i))
or = \x y -> Byte (\i -> Bit.or (unByte x i) (unByte y i))
xor = \x y -> Byte (\i -> Bit.xor (unByte x i) (unByte y i))

shiftr, shiftl :: Byte -> Byte
shiftr = \x -> Byte (\i -> unEight i b0 (unByte x e7) (unByte x e6) (unByte x e5) (unByte x e4) (unByte x e3) (unByte x e2) (unByte x e1))
shiftl = \x -> Byte (\i -> unEight i (unByte x e6) (unByte x e5) (unByte x e4) (unByte x e3) (unByte x e2) (unByte x e1) (unByte x e0) b0)

eqByte :: Byte -> Byte -> Bool
eqByte x y = Bool (
  andBit (eqBit (unByte x e0) (unByte y e0)) 
  (andBit (eqBit (unByte x e1) (unByte y e1))
  (andBit (eqBit (unByte x e2) (unByte y e2))
  (andBit (eqBit (unByte x e3) (unByte y e3))
  (andBit (eqBit (unByte x e4) (unByte y e4))
  (andBit (eqBit (unByte x e5) (unByte y e5))
  (andBit (eqBit (unByte x e6) (unByte y e6))
  (eqBit (unByte x e7) (unByte y e7))
  )))))))

newtype Four = Four { unFour :: forall x. x -> x -> x -> x -> x }

f0, f1, f2, f3 :: Four
f0 = Four (\_ _ _ x -> x)
f1 = Four (\_ _ x _ -> x)
f2 = Four (\_ x _ _ -> x)
f3 = Four (\x _ _ _ -> x)

newtype Nibble = Nibble { unNibble :: Four -> Bit }

x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, xA, xB, xC, xD, xE, xF :: Nibble
x0 = Nibble (\f -> unFour f b0 b0 b0 b0)
x1 = Nibble (\f -> unFour f b0 b0 b0 b1)
x2 = Nibble (\f -> unFour f b0 b0 b1 b0)
x3 = Nibble (\f -> unFour f b0 b0 b1 b1)
x4 = Nibble (\f -> unFour f b0 b1 b0 b0)
x5 = Nibble (\f -> unFour f b0 b1 b0 b1)
x6 = Nibble (\f -> unFour f b0 b1 b1 b0)
x7 = Nibble (\f -> unFour f b0 b1 b1 b1)
x8 = Nibble (\f -> unFour f b1 b0 b0 b0)
x9 = Nibble (\f -> unFour f b1 b0 b0 b1)
xA = Nibble (\f -> unFour f b1 b0 b1 b0)
xB = Nibble (\f -> unFour f b1 b0 b1 b1)
xC = Nibble (\f -> unFour f b1 b1 b0 b0)
xD = Nibble (\f -> unFour f b1 b1 b0 b1)
xE = Nibble (\f -> unFour f b1 b1 b1 b0)
xF = Nibble (\f -> unFour f b1 b1 b1 b1)

byte :: Nibble -> Nibble -> Byte
byte = \x y -> Byte (\z -> 
  unEight z 
    (unNibble x f3) 
    (unNibble x f2) 
    (unNibble x f1) 
    (unNibble x f0) 
    (unNibble y f3) 
    (unNibble y f2) 
    (unNibble y f1) 
    (unNibble y f0)
  )