module HiraneMain (main) where

import Prelude (($))
import Debug.Trace
import Embedded.Hirane.HS

import Embedded.Hirane.List
import Embedded.Hirane.DList (DList, fromList, toList)
import qualified Embedded.Hirane.DList as DList
import Embedded.Hirane.Nat (Nat, unNat, pred)
import qualified Embedded.Hirane.Nat as Nat
import Embedded.Hirane.Int (Int, unInt)
import qualified Embedded.Hirane.Int as Int
import Embedded.Hirane.Bool as Bool
import Embedded.Hirane.Product
import Embedded.Hirane.Bit
import Embedded.Hirane.Maybe
import Embedded.Hirane.Fix

split :: (a -> a -> Bool) -> a -> List a -> List (List a)
split = \eq y xs0 -> foldr
  (\x go d ->
    unBool (eq y x)
      (cons (toList d) (go DList.nil))
      (go (DList.snoc d x))
  )
  (\d -> cons (toList d) nil)
  xs0
  DList.nil

lines :: List Nat -> List (List Nat)
lines = split Nat.eq (Nat.pushBit b0 (Nat.pushBit b1 (Nat.pushBit b0 (Nat.pushBit b1 Nat.zero))))

-- assumes input is a digit
-- just take the four lowest bits
fromDigit :: Nat -> Nat
fromDigit = \x0 -> fix (\go n x ->
  unMaybe (pred n)
    (\n' -> unNat x
      (\x' -> Nat.pushBit b0 (go n' x'))
      (\x' -> Nat.pushBit b1 (go n' x'))
      Nat.zero)
    Nat.zero)
  (Nat.pushBit b0 (Nat.pushBit b0 (Nat.pushBit b1 Nat.zero)))
  x0

-- assumes input is between 0 and 9
-- just add 0b110000
toDigit :: Nat -> Nat
toDigit = Nat.add (Nat.pushBit b0 (Nat.pushBit b0 (Nat.pushBit b0 (Nat.pushBit b0 (Nat.pushBit b1 (Nat.pushBit b1 Nat.zero))))))

ten :: Nat
ten = Nat.pushBit b0 (Nat.pushBit b1 (Nat.pushBit b0 (Nat.pushBit b1 Nat.zero)))

readNat :: List Nat -> Nat
readNat = \xs -> foldr (\x go n -> go (Nat.add (Nat.mul n ten) (fromDigit x))) (\z -> z) xs Nat.zero

-- this produces the output reversed :(
showNat :: Nat -> List Nat
showNat = \x -> unBool (Nat.ifzero x) (cons (toDigit Nat.zero) nil) (reverse (unfoldr (\n -> unProd (Int.div n ten) (\q r ->
  unBool (Bool.and (Nat.ifzero q) (Nat.ifzero r))
    nilf
    (consf (toDigit r) q))) x))

sum :: List Nat -> Nat
sum = foldr Nat.add Nat.zero

readLine :: List Nat -> Prod Nat Nat
readLine = \xs -> unList (split Nat.eq (Nat.pushBit b0 (Nat.pushBit b0 (Nat.pushBit b0 (Nat.pushBit b0 (Nat.pushBit b0 (Nat.pushBit b1 Nat.zero)))))) xs)
  (\x xs -> unList xs
    (\_ xs -> unList xs
      (\_ xs -> unList xs
        (\y _ -> prod (readNat x) (readNat y))
        (prod Nat.zero Nat.zero)) -- fail
      (prod Nat.zero Nat.zero)) -- fail
    (prod Nat.zero Nat.zero)) -- fail
  (prod Nat.zero Nat.zero) -- fail

-- >>> from embedNat (Int.abs (Int.sub (to embedNat 2) (to embedNat 3)))
-- 7

main :: List Nat -> List Nat
main = \xs -> showNat (sum (unProd (unzip (map readLine (lines xs))) (\xs ys -> zipWith (\x y -> Int.abs (Int.sub x y)) (sort Nat.cmp xs) (sort Nat.cmp ys))))
