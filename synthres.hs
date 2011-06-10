
{--
# 
# Synthesize an arbitrary resistance from a base unit
# Eventually this could also give mean and variance numbers
#
# synthres is free software.  It comes without any warranty, to
# to the extent permitted by applicable law.  You can redistribute it
# and/or modify it under the terms of the Do What The Fuck You Want To
# Public License, Version 2, as published by Sam Hocevar.  See
# http://sam.zoy.org/wtfpl/COPYING for more details
#
--}

module Main where

import Data.List
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)
import Control.Parallel.Strategies (parMap,rdeepseq)
import Control.DeepSeq (rnf,NFData)
import System.Environment (getArgs)

data ResNet t = NilRes
              | ResM t
              | IntP [t]
              | PRes (ResNet t, ResNet t)
              | SRes (ResNet t, ResNet t)
    deriving (Eq, Show, Read)

instance (NFData a) => NFData (ResNet a) where
  rnf (NilRes) = ()
  rnf (ResM t) = rnf t
  rnf (IntP t) = rnf t
  rnf (PRes (t1,t2)) = rnf t1 `seq` rnf t2
  rnf (SRes (t1,t2)) = rnf t1 `seq` rnf t2

-- count up the number of resistors in a net
netSize NilRes = 0
netSize (ResM k) = round k
netSize (IntP k) = round $ sum k
netSize (PRes (x,y)) = netSize x + netSize y
netSize (SRes (x,y)) = netSize x + netSize y

-- compare the size of two nets
compNet n1 n2 = compare (netSize n1) (netSize n2)

-- combine two nets, either in series or in parallel
combNet isPar n1 n2
 | isPar     = PRes ( n1 , n2 )
 | otherwise = SRes ( n1 , n2 )

-- combine Maybe ResNet, ResNet
combMaybe isPar (Just n1,(n2,v2)) = Just $ combNet isPar n1 n2
combMaybe _     (Nothing,_)       = Nothing

-- perform series-parallel simplification
simplifyNet (PRes (NilRes,NilRes    )) = NilRes
simplifyNet (PRes (x     ,NilRes    )) = simplifyNet x
simplifyNet (PRes (NilRes,x         )) = simplifyNet x
simplifyNet (PRes (ResM k,IntP j    )) = IntP $ k:j
simplifyNet (PRes (IntP k,ResM j    )) = IntP $ j:k
simplifyNet (PRes (IntP k,IntP j    )) = IntP $ k++j
simplifyNet (PRes (x     ,y         )) = let sx = simplifyNet x
                                             sy = simplifyNet y
                                         in if x==sx && y==sy
                                             then PRes(x,y)
                                             else simplifyNet $ PRes(sx,sy)
simplifyNet (SRes (NilRes,NilRes    )) = NilRes
simplifyNet (SRes (x     ,NilRes    )) = simplifyNet x
simplifyNet (SRes (NilRes,x         )) = simplifyNet x
simplifyNet (SRes (ResM k,ResM j    )) = ResM $ k+j
simplifyNet (SRes (x     ,y         )) = let sx = simplifyNet x
                                             sy = simplifyNet y
                                         in if x==sx && y==sy
                                             then SRes(x,y)
                                             else simplifyNet $ SRes(sx,sy)
simplifyNet x                          = x

-- simplify and compute resistance
netValue = netValue'.simplifyNet

-- compute the resistance value of a net
-- expects a simplified net
netValue' NilRes = error "unhandled NilRes: simplify network first"
netValue' (ResM k) = k
netValue' (IntP k) = ((1/).sum.(map (1/))) k
netValue' (SRes (x,y)) = netValue' x + netValue' y
netValue' (PRes (x,y)) = (1/) $ (1/netValue' x) + (1/netValue' y)

-- generate partitions of n with minimum constituent m
genPart 1 _ = [[1]]
genPart n m = [n] : mrst
  where rest = zipWith (\x y->[x-y,y]) (repeat n) [m..div n 2]
        mrst = if rest == []
                then []
                else concatMap recFun rest
        recFun (x:y:[]) = zipWith (++) (genPart x y) (repeat [y])

-- generate all integer partitions of a number
intPartitions = (flip genPart) 1

-- turn integer partitions into net-value pairs
genRes n = zip rNets rVals
  where gR = intPartitions n
        rNets = map (IntP.(map toRational)) gR
        rVals = map netValue rNets

-- syntheize a resistor using direct resistance/conductance
-- coversion. Gives an answer quickly, but can be suboptimal
synthBasic iR err isPar
 | iR == 0   = NilRes
 | otherwise = rCons (nNet, rNet)
  where fR = toRational $ truncate (iR+err)
        rR = abs $ iR - fR
        nErr = err * iR / rR
        nNet = case (fR,isPar) of
                (0,_    ) -> NilRes
                (_,True ) -> IntP $ take (round fR) $ repeat 1
                (_,False) -> ResM fR
        rCons = if isPar then PRes else SRes
        rNet = if rR == 0 || rR <= err
                then NilRes
                else synthBasic (1/rR) nErr (not isPar)

-- synthesize a resistor by first generating the basic
-- synthesis and then searching for a better solution
-- using partitionns of a bounded resistor set
synthRes r unit err = if isNothing hlpRes
                       then bNet
                       else fromJust hlpRes
   where rNorm = r / unit
         bNet = synthBasic rNorm err False
         bound = netSize bNet
         hlpRes = sRHlp rNorm err bound False

-- helper for synthRes
sRHlp nR iErr bound isPar
  | bound <= 0 = Nothing
  | nR == 0    = Just NilRes
  | otherwise  = if netSize lResult > bound then Nothing else Just lResult
  where bNet = synthBasic nR iErr isPar
        testCand (n,vx) = case (compare (nR+iErr) v,compare (nR-iErr) v) of
                          (EQ,_ ) -> Just n
                          (_ ,EQ) -> Just n
                          (GT,LT) -> Just n
                          (LT,LT) -> Nothing
                          (GT,GT) -> combMaybe isPar (rNet,(nNet,undefined))
          where v = if isPar then 1/vx else vx
                xRst = nR - v
                wnR = toRational $ truncate (xRst+iErr)
                nnR = abs $ xRst - wnR
                nErr = iErr * nR / nnR
                nNet = case (wnR,isPar) of
                        (0,_    ) -> n
                        (_,True ) -> PRes (IntP $ take (round wnR) $ repeat 1,n)
                        (_,False) -> SRes (ResM wnR,n)
                rNet = if nnR == 0 || nnR <= iErr
                        then Just NilRes
                        else sRHlp (1/nnR) nErr (bound - netSize n) (not isPar)
        eqResVal (_,x) (_,y) = x == y
        pCands = nubBy eqResVal $ concat $ parMap rdeepseq genRes [1..bound]
        pResults = catMaybes $ parMap rdeepseq testCand pCands
        lResult = minimumBy compNet $ bNet : pResults

-- quick commandline interface
main = do
    [d,e] <- map (toRational.read) `fmap` getArgs
    let g = synthRes d e 1e-6
    putStrLn $ (show.simplifyNet) g
    putStrLn $ (show.netSize) g
    putStrLn.show $ netValue g

