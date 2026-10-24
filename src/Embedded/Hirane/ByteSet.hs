module Embedded.Hirane.ByteSet where

import Prelude ()

import Embedded.Hirane.Byte
import Embedded.Hirane.Bit
import Embedded.Hirane.Maybe
import Embedded.Hirane.Bool

newtype Unit = Unit { unUnit :: forall x. x -> x }
newtype Split a = Split { unSplit :: forall x. (a -> x) -> (a -> x) -> (a -> a -> x) -> x }

newtype ByteSet = ByteSet { unByteSet :: forall x. x -> (Split (Split (Split (Split (Split (Split (Split (Split Unit))))))) -> x) -> x }

disjointUnion1 :: (a -> a -> Maybe a) -> Split a -> Split a -> Maybe (Split a)
disjointUnion1 = \f x y -> 
  unSplit x
    (unSplit y
      (\x y -> mapMaybe (\z -> Split (\l _ _ -> l z)) (f x y))
      (\x y -> just (Split (\_ _ lr -> lr x y)))
      (\x y1 y2 -> mapMaybe (\z -> Split (\_ _ lr -> lr z y2)) (f x y1)))
    (unSplit y
      (\x y -> just (Split (\_ _ lr -> lr y x)))
      (\x y -> mapMaybe (\z -> Split (\_ r _ -> r z)) (f x y))
      (\x y1 y2 -> mapMaybe (\z -> Split (\_ _ lr -> lr y1 z)) (f x y2)))
    (unSplit y
      (\x1 x2 y -> mapMaybe (\z -> Split (\_ _ lr -> lr z x2)) (f x1 y))
      (\x1 x2 y -> mapMaybe (\z -> Split (\_ _ lr -> lr x1 z)) (f x2 y))
      (\x1 x2 y1 y2 -> mapMaybe2 (\z1 z2 -> Split (\_ _ lr -> lr z1 z2)) (f x1 y1) (f x2 y2)))

disjointUnion :: ByteSet -> ByteSet -> Maybe ByteSet
disjointUnion = \x y ->
  unByteSet x
    (just y)
    (unByteSet y
        (\_ -> just x)
        (\x0 y0 -> mapMaybe (\x -> ByteSet (\_ f -> f x)) 
          (disjointUnion1 
          (disjointUnion1
          (disjointUnion1 
          (disjointUnion1 

          (disjointUnion1 
          (disjointUnion1
          (disjointUnion1 
          (disjointUnion1 
          (\_ _ -> nothing))))))))
            x0
            y0)))

elem1 :: Bit -> (a -> Bool) -> Split a -> Bool
elem1 = \b f s -> unSplit s (\x -> unBit b (f x) false) (\y -> unBit b false (f y)) (\x y -> unBit b (f x) (f y))

elem :: Byte -> ByteSet -> Bool
elem = \x xs -> unByteSet xs
  false
  (elem1 (unByte x e7)
  (elem1 (unByte x e6)
  (elem1 (unByte x e5)
  (elem1 (unByte x e4)
  (elem1 (unByte x e3)
  (elem1 (unByte x e2)
  (elem1 (unByte x e1)
  (elem1 (unByte x e0)
  (\_ -> true)))))))))

emptyByteSet :: ByteSet
emptyByteSet = ByteSet (\x _ -> x)

singletonByteSet1 :: Bit -> a -> Split a
singletonByteSet1 = \b x ->
  unBit b (Split (\l _ _ -> l x)) (Split (\_ r _ -> r x))

singletonByteSet :: Byte -> ByteSet
singletonByteSet = \b ->
  ByteSet (\_ f -> f 
    (singletonByteSet1 (unByte b e7) 
    (singletonByteSet1 (unByte b e6) 
    (singletonByteSet1 (unByte b e5) 
    (singletonByteSet1 (unByte b e4) 
    (singletonByteSet1 (unByte b e3) 
    (singletonByteSet1 (unByte b e2) 
    (singletonByteSet1 (unByte b e1) 
    (singletonByteSet1 (unByte b e0) 
    (Unit (\x -> x))))))))))
    )

union1 :: (a -> a -> a) -> Split a -> Split a -> Split a
union1 = \f x y -> 
  unSplit x
    (unSplit y
      (\x y -> Split (\l _ _ -> l (f x y)))
      (\x y -> Split (\_ _ lr -> lr x y))
      (\x y1 y2 -> Split (\_ _ lr -> lr (f x y1) y2)))
    (unSplit y
      (\x y -> Split (\_ _ lr -> lr y x))
      (\x y -> Split (\_ r _ -> r (f x y)))
      (\x y1 y2 -> Split (\_ _ lr -> lr y1 (f x y2))))
    (unSplit y
      (\x1 x2 y -> Split (\_ _ lr -> lr (f x1 y) x2))
      (\x1 x2 y -> Split (\_ _ lr -> lr x1 (f x2 y)))
      (\x1 x2 y1 y2 -> Split (\_ _ lr -> lr (f x1 y1) (f x2 y2))))

union :: ByteSet -> ByteSet -> ByteSet
union = \x y ->
  unByteSet x
    y
    (unByteSet y
        (\_ -> x)
        (\x0 y0 -> ByteSet (\_ f -> f
          (union1 
          (union1
          (union1 
          (union1 

          (union1 
          (union1
          (union1 
          (union1 
          (\_ _ -> Unit (\x -> x)))))))))
            x0
            y0))))
