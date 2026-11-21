-- Parser combinators for parsing LL(1) deterministically
-- will fail with runtime error if non-LL(1) grammar is attempted
module Embedded.Hirane.Parser where

import Prelude ()

import Embedded.Hirane.List
import Embedded.Hirane.Maybe
import Embedded.Hirane.Bool
import Embedded.Hirane.Byte
import Embedded.Hirane.ByteSet

newtype Parser a = MkParser { unParser :: forall x. (Maybe a -> ByteSet -> (Byte -> List Byte -> (a -> ByteSet) -> ListF a (List Byte)) -> x) -> x}

-- parse nothing
-- like empty from Alternative
empty :: Parser a
empty = MkParser (\f -> f nothing emptyByteSet (\_ _ _ -> nilf))

-- parse exactly the empty string
-- like pure from Applicative
epsilon :: a -> Parser a
epsilon = \x -> MkParser (\f -> f (just x) emptyByteSet (\b bs _ -> consf x (cons b bs)))

symbol :: Byte -> Parser Byte
symbol = \s -> MkParser (\f -> f nothing (singletonByteSet s) (\b bs _ ->
  unBool (eqByte s b) (consf b bs) nilf)
  )

-- like <|> from Alternative
alt :: Parser a -> Parser a -> Parser a
alt = \p q -> 
  unMaybe (disjointUnion (unParser p (\_ x _ -> x)) (unParser q (\_ x _ -> x)))
    (\first -> 
      MkParser (\f -> f 
      (firstJust (unParser p (\x _ _ -> x)) (unParser q (\x _ _ -> x)))
      first
      (\b bs follow -> 
        unBool (elem b (unParser p (\_ x _ -> x))) (unParser p (\_ _ f -> f b bs follow)) 
        (unBool (elem b (unParser q (\_ x _ -> x))) (unParser q (\_ _ f -> f b bs follow))
        (unMaybe (unParser p (\x _ _ -> x)) 
          (\x -> 
            let followx = follow x in
            unMaybe (unParser q (\x _ _ -> x))
              (\y -> 
                let followy = follow y in
                -- if both can be empty, the resulting follow sets must be disjoint
                unBool (isJust (disjointUnion followx followy))
                  (unBool (elem b followx) 
                    (unParser p (\_ _ f -> f b bs follow))
                    (unBool (elem b followy)
                      (unParser q (\_ _ f -> f b bs follow))
                      nilf)) 
                  nilf)
              (unBool (elem b (follow x))
                (unParser p (\_ _ f -> f b bs follow))
                nilf)) 
          (unMaybe (unParser q (\x _ _ -> x))
            (\y -> unBool (elem b (follow y))
              (unParser q (\_ _ f -> f b bs follow))
              nilf)
            nilf))
        ))
    ))
    empty

-- like >>= from Monad
seq :: Parser a -> (a -> Parser b) -> Parser b
seq = \p k -> unParser p (\emp first pf ->
  unMaybe emp (\pe -> 
    unParser (k pe) (\keemp kefirst _ ->
      (MkParser (\f -> f 
        keemp
        (union kefirst first)
        (\b bs follow ->
          unListF (pf b bs (\_x -> {- assert _x == pe -} unMaybe keemp (\y -> union kefirst (follow y)) kefirst))
            (\x bbs ->
              unParser (k x) (\kemp _ kf ->
              unListF (uncons bbs)
                (\b' bs' -> kf b' bs' follow)
                (unMaybe kemp (\y -> consf y nil) nilf)))
            nilf)))))
    (MkParser (\f -> f nothing first (\b bs follow ->
      unListF (pf b bs (\x -> unParser (k x) (\_ x _ -> x)))
        (\x bbs ->
          unParser (k x) (\kemp _ kf ->
            unListF (uncons bbs)
              (\b' bs' -> kf b' bs' follow)
              (unMaybe kemp (\y -> consf y nil) nilf)))
        nilf)))
  )

many1 :: Parser a -> Parser (List a)
many1 = \p -> alt (seq p (\x -> epsilon (cons x nil))) (epsilon nil)

many2 :: Parser a -> Parser (List a)
many2 = \p -> alt (seq p (\x -> seq (many1 p) (\xs -> epsilon (cons x xs)))) (epsilon nil)

many, some :: Parser a -> Parser (List a)
many = \p -> alt (some p) (epsilon nil)
some = \p -> seq p (\x -> seq (many p) (\y -> epsilon (cons x y)))

parse :: Parser a -> List Byte -> Maybe a
parse = \p xs -> unParser p (\emp _ f -> 
  unListF (uncons xs) (\x xs' -> 
    unListF (f x xs' (\_ -> emptyByteSet))
      (\y xs'' -> unListF (uncons xs'') (\_ _ -> nothing) (just y))
      nothing) 
    emp)