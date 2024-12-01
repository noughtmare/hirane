module Embedded.Hirane.List where

import qualified Prelude ()

import Embedded.Hirane.Fix
import Embedded.Hirane.Product
import Embedded.Hirane.Ord

newtype List a = List { unList :: forall x. (a -> List a -> x) -> x -> x }

cons :: a -> List a -> List a
cons x xs = List (\c _ -> c x xs)

nil :: List a
nil = List (\_ n -> n)

singleton :: a -> List a
singleton = \x -> cons x nil

append :: List a -> List a -> List a
append = \xs ys -> foldr cons ys xs

foldr :: (a -> b -> b) -> b -> List a -> b
foldr = \k z -> fix (\foldr xs -> unList xs (\x xs' -> k x (foldr xs')) z)

map :: (a -> b) -> List a -> List b
map = \f -> foldr (\x -> cons (f x)) nil

newtype ListF a k = ListF { unListF :: forall x. (a -> k -> x) -> x -> x }

nilf :: ListF a k
nilf = ListF (\_ x -> x)

consf :: a -> k -> ListF a k
consf = \a k -> ListF (\c _ -> c a k)

unfoldr :: (b -> ListF a b) -> b -> List a
unfoldr = \uncons -> fix (\unfoldr b -> unListF (uncons b)
  (\a b -> cons a (unfoldr b))
  nil)

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith = \f xs ys -> unfoldr
  (\xys ->
    unProd xys (\xs ys ->
      unList xs (\x xs' ->
          unList ys (\y ys' -> consf (f x y) (prod xs' ys'))
            nilf)
        nilf))
  (prod xs ys)

insert :: (a -> a -> Ordering) -> a -> List a -> List a
insert = \cmp x -> fix (\insert ys ->
  unList ys
    (\y ys' ->
      unOrdering (cmp x y)
        (cons x (cons y ys'))
        (cons x (cons y ys'))
        (cons y (insert ys')))
    (cons x nil))

sort :: (a -> a -> Ordering) -> List a -> List a
sort = \cmp -> foldr (insert cmp) nil

unzip :: List (Prod a b) -> Prod (List a) (List b)
unzip = foldr (\x xs -> unProd x (\y z -> unProd xs (\ys zs -> prod (cons y ys) (cons z zs)))) (prod nil nil)

reverse :: List a -> List a
reverse = \xs -> foldr (\x go ys -> go (cons x ys)) (\z -> z) xs nil