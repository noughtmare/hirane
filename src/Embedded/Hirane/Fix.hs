module Embedded.Hirane.Fix where

import qualified Prelude ()

newtype Fix a = In { out :: Fix a -> a }

-- GHC's inliner will blow up if this is allowed to inline
{-# NOINLINE diag #-}
diag :: Fix a -> a
diag = \x -> out x x

fix :: (a -> a) -> a
fix = \f -> diag (In (\x -> f (out x x)))
