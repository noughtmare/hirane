module Embedded.Hirane.ByteSet where

import Prelude ()

import Embedded.Hirane.Byte as Byte
import Embedded.Hirane.Bit
import Embedded.Hirane.Maybe
import Embedded.Hirane.Bool
import Embedded.Hirane.List

newtype Unit = Unit { unUnit :: forall x. x -> x }
newtype Split a = Split { unSplit :: forall x. (a -> x) -> (a -> x) -> (a -> a -> x) -> x }

newtype ByteSet = ByteSet { unByteSet :: forall x. x -> (Split (Split (Split (Split  (Split (Split (Split (Split Unit))))))) -> x) -> x }

toList1 :: Eight -> (a -> List Byte) -> Split a -> List Byte
toList1 = \e f x ->
  unSplit x
    (\x1 -> map (setBit e) (f x1))
    (\x0 -> map (clearBit e) (f x0))
    (\x1 x0 -> append (map (clearBit e) (f x0)) (map (setBit e) (f x1)))

toList :: ByteSet -> List Byte
toList = \bs -> unByteSet bs nil 
  (toList1 e7 (toList1 e6 (toList1 e5 (toList1 e4
  (toList1 e3 (toList1 e2 (toList1 e1 (toList1 e0
  (\_ -> cons by00 nil)))))))))

disjointUnion1 :: (a -> a -> Maybe a) -> Split a -> Split a -> Maybe (Split a)
disjointUnion1 = \f x y -> 
  unSplit x
    (\x1 -> unSplit y
      (\y1 -> mapMaybe (\z1 -> Split (\l _ _ -> l z1)) (f x1 y1))
      (\y0 -> just (Split (\_ _ lr -> lr x1 y0)))
      (\y1 y0 -> mapMaybe (\z1 -> Split (\_ _ lr -> lr z1 y0)) (f x1 y1)))
    (\x0 -> unSplit y
      (\y1 -> just (Split (\_ _ lr -> lr y1 x0)))
      (\y0 -> mapMaybe (\z0 -> Split (\_ r _ -> r z0)) (f x0 y0))
      (\y1 y0 -> mapMaybe (\z0 -> Split (\_ _ lr -> lr y1 z0)) (f x0 y0)))
    (\x1 x0 -> unSplit y
      (\y1 -> mapMaybe (\z1 -> Split (\_ _ lr -> lr z1 x0)) (f x1 y1))
      (\y0 -> mapMaybe (\z0 -> Split (\_ _ lr -> lr x1 z0)) (f x0 y0))
      (\y1 y0 -> mapMaybe2 (\z1 z0 -> Split (\_ _ lr -> lr z1 z0)) (f x1 y1) (f x0 y0)))

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
elem1 = \b f s -> unSplit s
  (\x1 -> unBit b (f x1) false)
  (\x0 -> unBit b false (f x0))
  (\x1 x0 -> unBit b (f x1) (f x0))

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
    (\x -> unSplit y
      (\y -> Split (\l _ _ -> l (f x y)))
      (\y -> Split (\_ _ lr -> lr x y))
      (\y1 y2 -> Split (\_ _ lr -> lr (f x y1) y2)))
    (\x -> unSplit y
      (\y -> Split (\_ _ lr -> lr y x))
      (\y -> Split (\_ r _ -> r (f x y)))
      (\y1 y2 -> Split (\_ _ lr -> lr y1 (f x y2))))
    (\x1 x2 -> unSplit y
      (\y -> Split (\_ _ lr -> lr (f x1 y) x2))
      (\y -> Split (\_ _ lr -> lr x1 (f x2 y)))
      (\y1 y2 -> Split (\_ _ lr -> lr (f x1 y1) (f x2 y2))))

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
