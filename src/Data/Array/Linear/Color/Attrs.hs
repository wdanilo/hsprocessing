{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Array.Linear.Color.Attrs where

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

-- HSVA

h    = wrapped . x   
s    = wrapped . y   
hs   = wrapped . xy  
sh   = wrapped . yx  
v    = wrapped . z   
hv   = wrapped . xz  
vh   = wrapped . zx  
sv   = wrapped . yz  
vs   = wrapped . zy  
      
hsv  = wrapped . xyz 
hvs  = wrapped . xzy 
shv  = wrapped . yxz 
svh  = wrapped . yzx 
vhs  = wrapped . zxy 
vsh  = wrapped . zyx 
      
--a    = wrapped . w   
ah   = wrapped . wx  
ha   = wrapped . xw  
as   = wrapped . wy  
sa   = wrapped . yw  
av   = wrapped . wz  
va   = wrapped . zw  
      
ahs  = wrapped . wxy 
ash  = wrapped . wyx 
has  = wrapped . xwy 
hsa  = wrapped . xyw 
sah  = wrapped . ywx 
sha  = wrapped . yxw 
      
ahv  = wrapped . wxz 
avh  = wrapped . wzx 
hav  = wrapped . xwz 
hva  = wrapped . xzw 
vah  = wrapped . zwx 
vha  = wrapped . zxw 
      
avs  = wrapped . wzy 
asv  = wrapped . wyz 
vas  = wrapped . zwy 
vsa  = wrapped . zyw 
sav  = wrapped . ywz 
sva  = wrapped . yzw 
      
hsva = wrapped . xyzw
hvsa = wrapped . xzyw
shva = wrapped . yxzw
svha = wrapped . yzxw
vhsa = wrapped . zxyw
vsha = wrapped . zyxw
      
hsav = wrapped . xywz
hvas = wrapped . xzwy
shav = wrapped . yxwz
svah = wrapped . yzwx
vhas = wrapped . zxwy
vsah = wrapped . zywx
      
hasv = wrapped . xwyz
havs = wrapped . xwzy
sahv = wrapped . ywxz
savh = wrapped . ywzx
vahs = wrapped . zwxy
vash = wrapped . zwyx
      
ahsv = wrapped . wxyz
ahvs = wrapped . wxzy
ashv = wrapped . wyxz
asvh = wrapped . wyzx
avhs = wrapped . wzxy
avsh = wrapped . wzyx