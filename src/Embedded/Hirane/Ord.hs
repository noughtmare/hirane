module Embedded.Hirane.Ord where

import qualified Prelude ()

newtype Ordering = Ordering { unOrdering :: forall x. x -> x -> x -> x }

lt, eq, gt :: Ordering
lt = Ordering (\x _ _ -> x)
eq = Ordering (\_ x _ -> x)
gt = Ordering (\_ _ x -> x)

seq :: Ordering -> Ordering -> Ordering
seq = \x y -> unOrdering x lt y gt