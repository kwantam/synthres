
{--
# 
# this file is part of synthres
#
# synthres is free software.  It comes without any warranty, to
# to the extent permitted by applicable law.  You can redistribute it
# and/or modify it under the terms of the Do What The Fuck You Want To
# Public License, Version 2, as published by Sam Hocevar.  See
# http://sam.zoy.org/wtfpl/COPYING for more details
#
--}

module ResNetType where

import Data.List (partition)
import Control.DeepSeq (rnf,NFData)

data ResNet = NilRes
              | ResM Int
              | IntP [Int]
              | PRes (ResNet, ResNet)
              | SRes (ResNet, ResNet)
    deriving (Eq, Show, Read)

-- instance for NFData so that we can
-- apply the rdeepseq parallel strategy
-- when when run parMap
instance NFData ResNet where
  rnf (NilRes) = ()
  rnf (ResM t) = rnf t
  rnf (IntP t) = rnf t
  rnf (PRes (t1,t2)) = rnf t1 `seq` rnf t2
  rnf (SRes (t1,t2)) = rnf t1 `seq` rnf t2

-- count up the number of resistors in a net
netSize NilRes = 0
netSize (ResM k) = k
netSize (IntP k) = sum k
netSize (PRes (x,y)) = netSize x + netSize y
netSize (SRes (x,y)) = netSize x + netSize y

-- compare the size (in units) of two nets
compNet n1 n2 = compare (netSize n1) (netSize n2)

-- combine two nets, either in series or in parallel
combNet isPar n1 n2
 | isPar     = PRes ( n1 , n2 )
 | otherwise = SRes ( n1 , n2 )

-- combine Maybe ResNet, ResNet
combMaybe isPar (Just n1) n2 = Just $ combNet isPar n1 n2
combMaybe _     Nothing   _  = Nothing

-- perform series-parallel simplification
simplifyNet (IntP [k])                 = ResM k
simplifyNet (PRes (NilRes,NilRes    )) = NilRes
simplifyNet (PRes (x     ,NilRes    )) = simplifyNet x
simplifyNet (PRes (NilRes,x         )) = simplifyNet x
simplifyNet (PRes (ResM k,IntP j    )) = IntP $ k:j
simplifyNet (PRes (IntP k,ResM j    )) = IntP $ j:k
simplifyNet (PRes (IntP k,IntP j    )) = IntP $ k++j
simplifyNet (PRes (ResM k,ResM j    )) = IntP [k,j]
simplifyNet (PRes (x     ,y         )) = let sx = simplifyNet x
                                             sy = simplifyNet y
                                         in if x==sx && y==sy
                                             then PRes(x,y)
                                             else simplifyNet $ PRes(sx,sy)
simplifyNet (SRes (NilRes,NilRes    )) = NilRes
simplifyNet (SRes (x     ,NilRes    )) = simplifyNet x
simplifyNet (SRes (NilRes,x         )) = simplifyNet x
simplifyNet (SRes (ResM k,ResM j    )) = ResM $ k+j
simplifyNet (SRes (ResM j,SRes x))     = simplifyNet $ SRes (SRes x,ResM j)
simplifyNet (SRes (SRes (x,y),ResM j))
           | isResM x                  = simplifyNet $ SRes (ResM $ j + fromResM x,y)
           | isResM y                  = simplifyNet $ SRes (ResM $ j + fromResM y,x)
           | isSRes x                  = simplifyNet $ SRes (SRes (ResM j,y),x)
           | isSRes y                  = simplifyNet $ SRes (SRes (x,ResM j),y)
simplifyNet (SRes (SRes (j,k),SRes (x,y)))
           | length resMs == 2         = simplifyNet $ SRes (ResM (sum $ map fromResM resMs), SRes (nResMs!!0,nResMs!!1))
  where (resMs,nResMs) = partition isResM [j,k,x,y]
simplifyNet (SRes (x     ,y         )) = let sx = simplifyNet x
                                             sy = simplifyNet y
                                         in if x==sx && y==sy
                                             then SRes(x,y)
                                             else simplifyNet $ SRes(sx,sy)
simplifyNet x                          = x


-- is this element ResM?
isResM (ResM _) = True
isResM _        = False

-- is this element SRes?
isSRes (SRes _) = True
isSRes _        = False

-- extract value from ResM
fromResM (ResM x) = x

-- simplify and compute resistance
-- (make sure the type is as general as possible)
netValue :: (RealFrac a) => ResNet -> a
netValue = netValue'.simplifyNet

-- compute the resistance value of a net
-- expects a simplified net
-- (make sure the type is as general as possible)
netValue' :: (RealFrac a) => ResNet -> a
netValue' NilRes = error "unhandled NilRes: simplify network first"
netValue' (ResM k) = fromIntegral k
netValue' (IntP k) = ((1/).sum.(map ((1/).fromIntegral))) k
netValue' (SRes (x,y)) = netValue' x + netValue' y
netValue' (PRes (x,y)) = (1/) $ (1/netValue' x) + (1/netValue' y)

