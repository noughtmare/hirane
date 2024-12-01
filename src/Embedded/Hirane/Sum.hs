module Embedded.Hirane.Sum where

import qualified Prelude ()

newtype Zero = Zero { unZero :: forall x. x }
newtype Sum a b = Sum { unSum :: forall x. (a -> x) -> (b -> x) -> x }

inl :: a -> Sum a b
inl = \x -> Sum (\f _ -> f x)

inr :: b -> Sum a b
inr = \x -> Sum (\_ g -> g x)

newtype Sum3 a b c = Sum3 { unSum3 :: forall x. (a -> x) -> (b -> x) -> (c -> x) -> x }