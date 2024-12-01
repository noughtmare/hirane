module Embedded.Hirane.Product where

import qualified Prelude ()

newtype Prod a b = Prod { unProd :: forall x. (a -> b -> x) -> x }

fst :: Prod a b -> a
fst t = unProd t (\x _ -> x)

snd :: Prod a x -> x
snd t = unProd t (\_ y -> y)

prod :: a -> b -> Prod a b
prod x y = Prod (\f -> f x y)

-- Prod0
newtype Unit = Unit { unUnit :: forall x. x -> x }

newtype Prod3 a b c = Prod3 { unProd3 :: forall x. (a -> b -> c -> x) -> x }