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

module ResNetSynth where

import Data.List (nubBy, minimumBy)
import Data.Maybe (isNothing, fromJust, catMaybes)
import Control.Parallel.Strategies (parMap,rdeepseq,parListN)
import Control.Parallel (par,pseq)
import Control.DeepSeq (rnf)
import ResNetType

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
        rNets = map IntP gR
        rVals = map netValue rNets

-- infinite list of n-sized partitions
partNets = map genRes [1..]

-- syntheize a resistor using direct resistance/conductance
-- coversion. Gives an answer quickly, but can be suboptimal
synthBasic iR err isPar
 | iR == 0   = NilRes
 | otherwise = rCons (nNet, rNet)
  where (fR,rR) = if truncate (iR + err) /= truncate (iR - err)
                   then (ceiling iR,0)
                   else properFraction iR
        nErr = err * iR / rR
        nNet = case (fR,isPar) of
                (0,_    ) -> NilRes
                (_,True ) -> IntP $ replicate fR 1
                (_,False) -> ResM fR
        rCons = if isPar then PRes else SRes
        rNet = if rR <= err
                then NilRes
                else synthBasic (1/rR) nErr (not isPar)

-- nubCand is used in synthRes and sRHlp
nubCand = nubBy eqVal
  where eqVal (_,x) (_,y) = x == y

-- synthesize a resistor by first generating the basic
-- synthesis and then searching for a better solution
-- using partitions of a bounded resistor set
synthRes :: Rational -> Rational -> Rational -> ResNet
synthRes r unit err = if isNothing hlpRes
                       then bNet
                       else fromJust hlpRes
   where rNorm = r / unit
         bNet = synthBasic rNorm err False
         iBound = netSize bNet
         cBound = 25
         rCands = parListN (min cBound iBound) rdeepseq partNets `seq` take (min cBound iBound) partNets
         iCands = nubCand $ filter (\(_,x) -> x <= rNorm) $ concat rCands
         cNext (n,vx) = let k = rNorm - vx
                            nErr = rNorm * err / k
                            nx = synthBasic k nErr False
                         in netSize n + netSize nx
         nVals = parMap rdeepseq cNext iCands
         kBound = minimum $ iBound : nVals
         hlpRes = sRHlp rNorm err kBound False
-- note: we try to estimate a tight bound by taking one step of
-- the algorithm blindly and finding the best solution so far
-- this lets us have some chance of converging even for really
-- annoying ones like synthRes 999 1000 1e-6

-- helper for synthRes
sRHlp :: Rational -> Rational -> Int -> Bool -> Maybe ResNet
sRHlp nR iErr bound isPar
  | bound <= 0 = Nothing                -- can't make resistance from nothing
  | nR > fromIntegral bound = Nothing   -- not enough resistors to make nR
  | nR == 0    = Just NilRes            -- a zero R or G is just NilRes
  | (not isPar) &&                      -- The largest partition resistance
    nR >= 1 + fromIntegral bound / 4 =  -- is bound/4, so just put any excess
      let nComb = floor $ nR - fromIntegral bound / 4 -- in series; makes bound
          nCombR = fromIntegral nComb   -- small fast, which is a big speedup.
          nnR = nR - nCombR             -- This doesn't work when isPar because
          nErr = iErr * nR / nnR        -- the largest conductance equals bound.
      in combMaybe False (sRHlp nnR nErr (bound - nComb) False) (ResM nComb)
  | otherwise  = if netSize lResult > bound then Nothing else Just lResult
  where bNet = synthBasic nR iErr isPar
        testCand (n,vx) = case (compare (nR+iErr) v,compare (nR-iErr) v) of
                          (EQ,_ ) -> Just n
                          (_ ,EQ) -> Just n
                          (GT,LT) -> Just n
                          (LT,LT) -> Nothing
                          (GT,GT) -> combMaybe isPar rNet nNet
          where v = if isPar then 1/vx else vx
                xRst = nR - v
                (wnR,nnR) = if truncate (xRst+iErr) /= truncate (xRst-iErr)
                             then (ceiling xRst,0)
                             else properFraction xRst
                nErr = iErr * nR / nnR
                nNet = case (wnR,isPar) of
                        (0,_    ) -> n
                        (_,True ) -> PRes (IntP $ replicate wnR 1,n)
                        (_,False) -> SRes (ResM wnR,n)
                rNet = if nnR <= iErr
                        then Just NilRes
                        else sRHlp (1/nnR) nErr (bound - netSize n - wnR) (not isPar)
        rCands = parListN bound rdeepseq partNets `seq` take bound partNets
        pCands = nubCand $ concat rCands
        pResults = catMaybes $ parMap rdeepseq testCand pCands
        lResult = minimumBy compNet $ bNet : pResults
