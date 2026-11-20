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

-- data Exp = Var String | Lam String Exp | App Exp Exp
newtype Exp = MkExp { unExp :: forall x. (List Byte -> x) -> (List Byte -> Exp -> x) -> (Exp -> Exp -> x) -> x }

var :: List Byte -> Exp
var x = MkExp (\var _ _ -> var x)

lam :: List Byte -> Exp -> Exp
lam x y = MkExp (\_ lam _ -> lam x y)

app :: Exp -> Exp -> Exp
app x y = MkExp (\_ _ app -> app x y)

lexeme :: Parser a -> Parser a
lexeme x = x
-- lexeme = \p -> seq (many (symbol space)) (\_ -> p)

parens :: Parser a -> Parser a
parens = \p ->
  seq (symbol openParen) (\_ ->
  seq p (\x ->
  seq (lexeme (symbol closeParen)) (\_ ->
  epsilon x)))

seq0 :: Parser a -> Parser b -> Parser b
seq0 x y = seq x (\_ -> y)

pvar, plam :: Parser Exp
pvar = seq identifier (\x -> epsilon (var x))
plam = seq (symbol backslash) 
  (\_ -> seq (lexeme identifier) 
  (\x -> seq (lexeme (seq (symbol dash) (\_ -> symbol rangle)))
  (\_ -> seq (lexeme exp) 
  (\y -> epsilon (lam x y)))))

exp :: Parser Exp
exp = seq exp1 (\f -> alt (seq (symbol space) (\_ -> seq exp (\x -> epsilon (app f x)))) (epsilon f))

exp1 :: Parser Exp
exp1 = alt pvar (parens exp1) -- (lexeme (alt plam exp))

-- Bunch of test cases resulting from my investigation into a, now fixed, bug.
-- >>> import Embedded.Hirane.Maybe as H
-- >>> import Embedded.Hirane.HS
-- >>> from embedBool (H.isJust (parse identifier (to embedString "x")))
-- >>> from embedBool (H.isJust (parse (many (alt (symbol c0) (symbol c9))) (to embedString "0")))
-- >>> from embedBool (H.isJust (parse lowercase (to embedString "c")))
-- True
-- True
-- True
-- >>> from embedBool (H.isJust (parse (some (symbol cx)) (to embedString "xxx")))
-- >>> from embedBool (H.isJust (parse (seq0 (some (symbol cx)) (symbol cA)) (to embedString "xA")))
-- >>> from embedBool (H.isJust (parse (seq0 (seq0 (symbol cx) (alt (symbol cx) (epsilon cx))) (symbol cA)) (to embedString "xA")))
-- >>> from embedBool (H.isJust (parse (seq0 (symbol cx) (seq0 (alt (symbol cx) (epsilon cx)) (symbol cA))) (to embedString "xA")))
-- True
-- False
-- False
-- True
-- >>> from embedBool (H.isJust (parse (many1 (alt (symbol c0) (symbol c9))) (to embedString "0")))
-- >>> from embedBool (H.isJust (parse (many2 (alt (symbol c0) (symbol c9))) (to embedString "0")))
-- True
-- True
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
-- True
-- True
-- True
-- True
-- True
-- True
-- True
-- >>> import Embedded.Hirane.ByteSet as BS
-- >>> import Embedded.Hirane.Bool
-- >>> from embedBool (fromMaybe false (mapMaybe (elem cA) (disjointUnion (singletonByteSet cA) (singletonByteSet cA))))
-- >>> from embedBool (fromMaybe false (mapMaybe (elem cA) (disjointUnion (singletonByteSet cA) (singletonByteSet cB))))
-- >>> from embedBool (fromMaybe false (mapMaybe (elem cA) (disjointUnion (singletonByteSet cA) (singletonByteSet cC))))
-- >>> from embedBool (fromMaybe false (mapMaybe (elem cA) (disjointUnion (singletonByteSet cA) (singletonByteSet cD))))
-- >>> from embedBool (fromMaybe false (mapMaybe (elem cA) (disjointUnion (singletonByteSet cA) (singletonByteSet cE))))
-- >>> from embedBool (fromMaybe false (mapMaybe (elem cA) (disjointUnion (singletonByteSet cA) (singletonByteSet cF))))
-- >>> from embedBool (fromMaybe false (mapMaybe (elem cA) (disjointUnion (singletonByteSet cA) (singletonByteSet cG))))
-- False
-- True
-- True
-- True
-- True
-- True
-- True
-- >>> from embedString (toList (singletonByteSet cA))
-- >>> from embedString (fromMaybe nil (mapMaybe toList (disjointUnion (singletonByteSet cA) (singletonByteSet cA))))
-- >>> from embedString (fromMaybe nil (mapMaybe toList (disjointUnion (singletonByteSet cA) (singletonByteSet cB))))
-- >>> from embedString (fromMaybe nil (mapMaybe toList (disjointUnion (singletonByteSet cA) (singletonByteSet cC))))
-- >>> from embedString (fromMaybe nil (mapMaybe toList (disjointUnion (singletonByteSet cA) (singletonByteSet cD))))
-- >>> from embedString (fromMaybe nil (mapMaybe toList (disjointUnion (singletonByteSet cA) (singletonByteSet cE))))
-- >>> from embedString (fromMaybe nil (mapMaybe toList (disjointUnion (singletonByteSet cA) (singletonByteSet cF))))
-- >>> from embedString (fromMaybe nil (mapMaybe toList (disjointUnion (singletonByteSet cA) (singletonByteSet cG))))
-- >>> from embedBool (elem cA (singletonByteSet cA))
-- "A"
-- ""
-- "AB"
-- "AC"
-- "AD"
-- "AE"
-- "AF"
-- "AG"
-- True
-- >>> from embedBool (elem cA (union (singletonByteSet cA) (singletonByteSet cA)))
-- >>> from embedBool (elem cA (union (singletonByteSet cA) (singletonByteSet cB)))
-- >>> from embedBool (elem cA (union (singletonByteSet cA) (singletonByteSet cC)))
-- >>> from embedBool (elem cA (union (singletonByteSet cA) (singletonByteSet cD)))
-- >>> from embedBool (elem cA (union (singletonByteSet cA) (singletonByteSet cE)))
-- >>> from embedBool (elem cA (union (singletonByteSet cA) (singletonByteSet cF)))
-- >>> from embedBool (elem cA (union (singletonByteSet cA) (singletonByteSet cG)))
-- True
-- True
-- True
-- True
-- True
-- True
-- True
