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
  unParser p (\pemp pfirst pf ->
  unParser q (\qemp qfirst qf ->
  unMaybe (disjointUnion pfirst qfirst)
    (\first -> 
      MkParser (\f -> f 
      (firstJust pemp qemp)
      first
      (\b bs follow -> 
        unBool (elem b pfirst) (pf b bs follow)
        (unBool (elem b qfirst) (qf b bs follow)
        (unBool (unMaybe pemp (\x -> elem b (follow x)) false)
          (unBool (unMaybe qemp (\y -> elem b (follow y)) false) 
            nilf -- follow ambiguity
            (pf b bs follow))
          (unBool (unMaybe qemp (\y -> elem b (follow y)) false) 
            (qf b bs follow)
            nilf))
        ))
    ))
    empty)) -- first ambiguity

-- like >>= from Monad
seq :: Parser a -> (a -> Parser b) -> Parser b
seq = \p k -> unParser p (\emp first pf ->
  let k' follow = 
        (\x bbs ->
          unParser (k x) (\kemp _ kf ->
          unListF (uncons bbs)
            (\b' bs' -> kf b' bs' follow)
            (unMaybe kemp (\y -> consf y nil) nilf)))
  in
  unMaybe emp (\pe -> 
    unParser (k pe) (\keemp kefirst _ ->
      (MkParser (\f -> f 
        keemp
        (union kefirst first)
        (\b bs follow ->
          unListF (pf b bs (\_x -> 
              -- assert _x == pe
              unMaybe keemp (\kres -> union kefirst (follow kres)) kefirst))
            (k' follow)
            nilf
            )))))
    (MkParser (\f -> f nothing first (\b bs follow ->
      unListF (pf b bs (\pres -> unParser (k pres) (\kemp kfirst _ -> unMaybe kemp (\kres -> union kfirst (follow kres)) kfirst)))
        (k' follow)
        nilf
        )))
  )

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