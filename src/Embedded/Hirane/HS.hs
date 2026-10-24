module Embedded.Hirane.HS where

import Embedded.Hirane.Nat as Hirane
import Embedded.Hirane.Bit as Hirane
import qualified Embedded.Hirane.Bool as Hirane
import Embedded.Hirane.Byte as Hirane
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

embedBit :: Iso Bool Hirane.Bit
embedBit = Iso
  { to = \b -> case b of
    False -> b0
    True -> b1
  , from = \b -> unBit b True False
  }

embedBool :: Iso Bool Hirane.Bool
embedBool = Iso
  { to = \b -> if b then Hirane.true else Hirane.false
  , from = \b -> Hirane.unBool b True False
  }

orBit :: Bits a => Int -> Bool -> a -> a
orBit _ False x = x
orBit i True x = setBit x i

embedChar :: Iso Char Hirane.Byte
embedChar = Iso
  { to = \x -> Hirane.Byte (\e -> unEight e 
    (to embedBit (testBit (ord x) 7))
    (to embedBit (testBit (ord x) 6))
    (to embedBit (testBit (ord x) 5))
    (to embedBit (testBit (ord x) 4))
    (to embedBit (testBit (ord x) 3))
    (to embedBit (testBit (ord x) 2))
    (to embedBit (testBit (ord x) 1))
    (to embedBit (testBit (ord x) 0)))
  , from = \b -> chr
    (orBit 7 (from embedBit (unByte b e7))
    (orBit 6 (from embedBit (unByte b e6))
    (orBit 5 (from embedBit (unByte b e5))
    (orBit 4 (from embedBit (unByte b e4))
    (orBit 3 (from embedBit (unByte b e3))
    (orBit 2 (from embedBit (unByte b e2))
    (orBit 1 (from embedBit (unByte b e1))
    (orBit 0 (from embedBit (unByte b e0))
    0))))))))
  }

embedString :: Iso String (Hirane.List Hirane.Byte)
embedString = Iso
  { to = \xs -> case xs of
  [] -> Hirane.nil
  (x:xs) -> Hirane.cons (to embedChar x) (to embedString xs)
  , from = \xs ->
  Hirane.unList xs
    (\c xs' -> from embedChar c : from embedString xs')
    ""
  }
