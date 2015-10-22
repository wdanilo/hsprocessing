{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.Array.Linear.Color.Attrs.RGBA where

import Prologue
import Data.Array.Linear


-- === Attributes ===

-- TODO[WD]: use TemplateHaskell to generate attributes

-- RGBA

r    = wrapped . x   
g    = wrapped . y   
rg   = wrapped . xy  
gr   = wrapped . yx  
b    = wrapped . z   
rb   = wrapped . xz  
br   = wrapped . zx  
gb   = wrapped . yz  
bg   = wrapped . zy  

rgb  = wrapped . xyz 
rbg  = wrapped . xzy 
grb  = wrapped . yxz 
gbr  = wrapped . yzx 
brg  = wrapped . zxy 
bgr  = wrapped . zyx 

a    = wrapped . w   
ar   = wrapped . wx  
ra   = wrapped . xw  
ag   = wrapped . wy  
ga   = wrapped . yw  
ab   = wrapped . wz  
ba   = wrapped . zw  

arg  = wrapped . wxy 
agr  = wrapped . wyx 
rag  = wrapped . xwy 
rga  = wrapped . xyw 
gar  = wrapped . ywx 
gra  = wrapped . yxw 

arb  = wrapped . wxz 
abr  = wrapped . wzx 
rab  = wrapped . xwz 
rba  = wrapped . xzw 
bar  = wrapped . zwx 
bra  = wrapped . zxw 

abg  = wrapped . wzy 
agb  = wrapped . wyz 
bag  = wrapped . zwy 
bga  = wrapped . zyw 
gab  = wrapped . ywz 
gba  = wrapped . yzw 

rgba = wrapped . xyzw
rbga = wrapped . xzyw
grba = wrapped . yxzw
gbra = wrapped . yzxw
brga = wrapped . zxyw
bgra = wrapped . zyxw

rgab = wrapped . xywz
rbag = wrapped . xzwy
grab = wrapped . yxwz
gbar = wrapped . yzwx
brag = wrapped . zxwy
bgar = wrapped . zywx

ragb = wrapped . xwyz
rabg = wrapped . xwzy
garb = wrapped . ywxz
gabr = wrapped . ywzx
barg = wrapped . zwxy
bagr = wrapped . zwyx

argb = wrapped . wxyz
arbg = wrapped . wxzy
agrb = wrapped . wyxz
agbr = wrapped . wyzx
abrg = wrapped . wzxy
abgr = wrapped . wzyx
