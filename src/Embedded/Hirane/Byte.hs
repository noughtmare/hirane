module Embedded.Hirane.Byte where

import qualified Prelude ()

import qualified Embedded.Hirane.Bit as Bit
import Embedded.Hirane.Bit (Bit, b0, b1)
import Embedded.Hirane.Maybe
import Embedded.Hirane.Nat

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