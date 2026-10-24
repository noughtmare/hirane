module Embedded.Hirane.Maybe where

import qualified Prelude ()

import Embedded.Hirane.Bool

newtype Maybe a = Maybe { unMaybe :: forall x. (a -> x) -> x -> x }

nothing :: Maybe a
nothing = Maybe (\_ x -> x)

just :: a -> Maybe a
just = \x -> Maybe (\f _ -> f x)

fromMaybe :: a -> Maybe a -> a
fromMaybe = \x m -> unMaybe m (\x -> x) x

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe = \f m -> unMaybe m (\x -> just (f x)) nothing

mapMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
mapMaybe2 = \f x y -> unMaybe x
  (\x -> unMaybe y
    (\y -> just (f x y))
    nothing)
  nothing

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe = \x k -> unMaybe x k nothing

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust = \x y -> unMaybe x just y

isNothing :: Maybe a -> Bool
isNothing = \x -> unMaybe x (\_ -> false) true

isJust :: Maybe a -> Bool
isJust = \x -> unMaybe x (\_ -> true) false