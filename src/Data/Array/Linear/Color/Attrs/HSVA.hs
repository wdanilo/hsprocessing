{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Data.Array.Linear.Color.Attrs.HSVA where

import Prologue
import Data.Array.Linear


-- === Attributes ===

-- TODO[WD]: use TemplateHaskell to generate attributes

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