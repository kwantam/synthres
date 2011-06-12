
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

module Main where

import Data.List (nubBy, minimumBy)
import Data.Maybe (isNothing, fromJust, catMaybes)
import Control.Parallel.Strategies (parMap,rdeepseq)
import System.Environment (getArgs)
import IO (hPutStrLn,stderr)
import ResNetType
import ResNetSVG

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

-- syntheize a resistor using direct resistance/conductance
-- coversion. Gives an answer quickly, but can be suboptimal
synthBasic iR err isPar
 | iR == 0   = NilRes
 | otherwise = rCons (nNet, rNet)
  where fR = truncate (iR+err)
        rR = abs $ iR - fromIntegral fR
        nErr = err * iR / rR
        nNet = case (fR,isPar) of
                (0,_    ) -> NilRes
                (_,True ) -> IntP $ replicate fR 1
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
                wnR = truncate (xRst+iErr)
                nnR = abs $ xRst - fromIntegral wnR
                nErr = iErr * nR / nnR
                nNet = case (wnR,isPar) of
                        (0,_    ) -> n
                        (_,True ) -> PRes (IntP $ replicate wnR 1,n)
                        (_,False) -> SRes (ResM wnR,n)
                rNet = if nnR == 0 || nnR <= iErr
                        then Just NilRes
                        else sRHlp (1/nnR) nErr (bound - netSize n) (not isPar)
        eqResVal (_,x) (_,y) = x == y
        pCands = nubBy eqResVal $ concat $ parMap rdeepseq genRes [1..bound]
        pResults = catMaybes $ parMap rdeepseq testCand pCands
        lResult = minimumBy compNet $ bNet : pResults

-- quick commandline interface
-- information goes out on stderr, SVG rendition on stdout
main = do
    [u,r] <- map (toRational.read) `fmap` getArgs
    let g = synthRes r u 1e-6
    hPutStrLn stderr $ (show.simplifyNet) g
    hPutStrLn stderr $ (show.netSize) g
    (hPutStrLn stderr) $ show (netValue g :: Rational) ++ " (" ++ show (netValue g :: Double) ++ ")"
    putStrLn $ netToSVG g r u

