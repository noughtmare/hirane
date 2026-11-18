module Embedded.Hirane.ASCII where

import Prelude ()

import Embedded.Hirane.Byte

space :: Byte
space = byte x2 x1

openParen, closeParen :: Byte
openParen = byte x2 x8
closeParen = byte x2 x9

comma :: Byte
comma = byte x2 xC

c0, c1, c2, c3, c4, c5, c6, c7, c8, c9 :: Byte
c0 = byte x3 x0
c1 = byte x3 x1
c2 = byte x3 x2
c3 = byte x3 x3
c4 = byte x3 x4
c5 = byte x3 x5
c6 = byte x3 x6
c7 = byte x3 x7
c8 = byte x3 x8
c9 = byte x3 x9

colon :: Byte
colon = byte x3 xA

equals :: Byte
equals = byte x3 xD

cA, cB, cC, cD, cE, cF, cG, cH, cI, cJ, cK, cL, cM, cN, cO, cP, cQ, cR, cS, cT, cU, cV, cW, cX, cY, cZ :: Byte
cA = byte x4 x1
cB = byte x4 x2
cC = byte x4 x3
cD = byte x4 x4
cE = byte x4 x5
cF = byte x4 x6
cG = byte x4 x7
cH = byte x4 x8
cI = byte x4 x9
cJ = byte x4 xA
cK = byte x4 xB
cL = byte x4 xC
cM = byte x4 xD
cN = byte x4 xE
cO = byte x4 xF
cP = byte x5 x0
cQ = byte x5 x1
cR = byte x5 x2
cS = byte x5 x3
cT = byte x5 x4
cU = byte x5 x5
cV = byte x5 x6
cW = byte x5 x7
cX = byte x5 x8
cY = byte x5 x9
cZ = byte x5 xA

ca, cb, cc, cd, ce, cf, cg, ch, ci, cj, ck, cl, cm, cn, co, cp, cq, cr, cs, ct, cu, cv, cw, cx, cy, cz :: Byte
ca = byte x6 x1
cb = byte x6 x2
cc = byte x6 x3
cd = byte x6 x4
ce = byte x6 x5
cf = byte x6 x6
cg = byte x6 x7
ch = byte x6 x8
ci = byte x6 x9
cj = byte x6 xA
ck = byte x6 xB
cl = byte x6 xC
cm = byte x6 xD
cn = byte x6 xE
co = byte x6 xF
cp = byte x7 x0
cq = byte x7 x1
cr = byte x7 x2
cs = byte x7 x3
ct = byte x7 x4
cu = byte x7 x5
cv = byte x7 x6
cw = byte x7 x7
cx = byte x7 x8
cy = byte x7 x9
cz = byte x7 xA
