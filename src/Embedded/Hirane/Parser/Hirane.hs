module Embedded.Hirane.Parser.Hirane where

import Prelude ()

import Embedded.Hirane.List
import Embedded.Hirane.Byte
import Embedded.Hirane.Parser
import Embedded.Hirane.ASCII
import Embedded.Hirane.HS ()

lowercase :: Parser Byte
lowercase =
  let c x = alt (symbol x)
  in 
   c ca (c cb (c cc (c cd (c ce
  (c cf (c cg (c ch (c ci (c cj
  (c ck (c cl (c cm (c cn (c co
  (c cp (c cq (c cr (c cs (c ct
  (c cu (c cv (c cw (c cx (c cy
  (symbol cz
  )))))))))))))))))))))))))

uppercase :: Parser Byte
uppercase = 
  let c x = alt (symbol x) in
   c cA (c cB (c cC (c cD (c cE
  (c cF (c cG (c cH (c cI (c cJ
  (c cK (c cL (c cM (c cN (c cO
  (c cP (c cQ (c cR (c cS (c cT
  (c cU (c cV (c cW (c cX (c cY
  (symbol cZ
  )))))))))))))))))))))))))

digit :: Parser Byte
digit =
  let c x = alt (symbol x) in
   c c0 (c c1 (c c2 (c c3 (c c4 
  (c c5 (c c6 (c c7 (c c8 
  (symbol c9)
  ))))))))

identifier :: Parser (List Byte)
identifier = seq lowercase (\x -> seq (many (alt lowercase (alt uppercase digit))) (\xs -> epsilon (cons x xs)))

-- There's a bug somewhere, because this fails:
-- >>> import Embedded.Hirane.Maybe as H
-- >>> import Embedded.Hirane.HS
-- >>> from embedBool (H.isJust (parse (many (alt (symbol c0) (symbol c9))) (to embedString "0")))
-- False
-- >>> from embedBool (H.isJust (parse (many1 (alt (symbol c0) (symbol c9))) (to embedString "0")))
-- >>> from embedBool (H.isJust (parse (many2 (alt (symbol c0) (symbol c9))) (to embedString "0")))
-- False
-- False
-- >>> from embedBool (H.isJust (parse (alt (alt (symbol c0) (symbol c1)) (epsilon (byte x0 x0))) (to embedString "0")))
-- >>> from embedBool (H.isJust (parse (alt (alt (symbol c0) (symbol c2)) (epsilon (byte x0 x0))) (to embedString "0")))
-- >>> from embedBool (H.isJust (parse (alt (alt (symbol c0) (symbol c3)) (epsilon (byte x0 x0))) (to embedString "0")))
-- >>> from embedBool (H.isJust (parse (alt (alt (symbol c0) (symbol c4)) (epsilon (byte x0 x0))) (to embedString "0")))
-- >>> from embedBool (H.isJust (parse (alt (alt (symbol c0) (symbol c5)) (epsilon (byte x0 x0))) (to embedString "0")))
-- >>> from embedBool (H.isJust (parse (alt (alt (symbol c0) (symbol c6)) (epsilon (byte x0 x0))) (to embedString "0")))
-- >>> from embedBool (H.isJust (parse (alt (alt (symbol c0) (symbol c7)) (epsilon (byte x0 x0))) (to embedString "0")))
-- >>> from embedBool (H.isJust (parse (alt (alt (symbol c0) (symbol c8)) (epsilon (byte x0 x0))) (to embedString "0")))
-- >>> from embedBool (H.isJust (parse (alt (alt (symbol c0) (symbol c9)) (epsilon (byte x0 x0))) (to embedString "0")))
-- True
-- True
-- False
-- True
-- False
-- False
-- False
-- True
-- False
-- >>> import Embedded.Hirane.ByteSet as BS
-- >>> import Embedded.Hirane.Bool
-- >>> from embedBool (fromMaybe false (mapMaybe (elem cA) (disjointUnion (singletonByteSet cA) (singletonByteSet cB))))
-- >>> from embedBool (elem cA (singletonByteSet cA))
-- False
-- True

-- So the bug is probably in disjointUnion