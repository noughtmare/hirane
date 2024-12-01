module Embedded.Hirane.Maybe where

import qualified Prelude ()

newtype Maybe a = Maybe { unMaybe :: forall x. (a -> x) -> x -> x }

nothing :: Maybe a
nothing = Maybe (\_ x -> x)

just :: a -> Maybe a
just = \x -> Maybe (\f _ -> f x)

fromMaybe :: a -> Maybe a -> a
fromMaybe = \x m -> unMaybe m (\x -> x) x

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe = \f m -> unMaybe m (\x -> just (f x)) nothing