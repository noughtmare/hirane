module Main where

import qualified Embedded.Hirane.Nat as Hirane
import qualified Embedded.Hirane.List as Hirane
import qualified HiraneMain as Hirane
import Data.Bits
import Data.Char
import Debug.Trace
import Embedded.Hirane.HS

main :: IO ()
main = interact ((++ "\n") . from embedString . Hirane.main . to embedString)
