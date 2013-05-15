{-# LANGUAGE NoMonomorphismRestriction #-}
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

import Data.List (nubBy, minimumBy, foldl', foldl1')
import Data.Maybe (isNothing, fromJust, catMaybes)
import Control.Parallel.Strategies (parMap,rdeepseq,parListN)
import Control.DeepSeq (NFData(..))
import qualified Data.Map as DM
import Control.Monad.State (State(..), get, put, runState)
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
genRes n = zip rVals rNets
  where gR = intPartitions n
        rNets = map IntP gR
        rVals = map netValue rNets

-- infinite list of n-sized partitions
partNets :: [[(Rational,ResNet)]]
partNets = map genRes [1..]

-- syntheize a resistor using direct resistance/conductance
-- coversion. Gives an answer quickly, but can be suboptimal
synthBasic iR err isPar
 | iR == 0   = NilRes
 | otherwise = rCons (nNet, rNet)
  where (fR,rR) = if truncate (iR*(1+err)) /= truncate (iR*(1-err))
                   then (round iR,0)
                   else properFraction iR
        nErr = err * iR / rR
        nNet = case (fR,isPar) of
                (0,_    ) -> NilRes
                (_,True ) -> IntP $ replicate fR 1
                (_,False) -> ResM fR
        rCons = if isPar then PRes else SRes
        rNet = if rR <= (err * iR)
                then NilRes
                else synthBasic (1/rR) nErr (not isPar)

-- nubCand is used in synthRes and sRHlp
nubCand = DM.toAscList . DM.fromListWith (flip const)

-- when doing synthesis, use this as a hard upper
-- bound for the size of allResNets we're willing
-- to ask for
cBoundAll = 12

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
         cBound1 = 25
         rCands1 = parListN (min cBound1 iBound) rdeepseq partNets `seq` take (min cBound1 iBound) partNets
         (rCands2,st) = allResUpTo' DM.empty $ min cBoundAll iBound
         iCands = nubCand . filter ((rNorm>=) . fst) . concat $ rCands2 : rCands1
         cNext (vx,n) = let k = rNorm - vx
                            nErr = rNorm * err / k
                            nx = synthBasic k nErr False
                         in netSize n + netSize nx
         nVals = parMap rdeepseq cNext iCands
         kBound = minimum $ iBound : nVals
         hlpRes = sRHlp rNorm err kBound False st
-- note: we try to estimate a tight bound by taking one step of
-- the algorithm blindly and finding the best solution so far
-- this lets us have some chance of converging even for really
-- annoying ones like synthRes 999 1000 1e-6

-- helper for synthRes
sRHlp nR iErr bound isPar st
  | bound <= 0 = Nothing                -- can't make resistance from nothing
  | nR > fromIntegral bound = Nothing   -- not enough resistors to make nR
  | iErr >= 1 = Just NilRes             -- good enough (iErr >= 100% of nR)
  | nR == 0 = Just NilRes               -- a zero R or G is just NilRes
  | (not isPar) &&                      -- The largest partition resistance
    nR >= 1 + fromIntegral bound / 4 =  -- is bound/4, so just put any excess
      let nComb = floor $ nR - fromIntegral bound / 4 -- in series; makes bound
          nCombR = fromIntegral nComb   -- small fast, which is a big speedup.
          nnR = nR - nCombR             -- This doesn't work when isPar because
          nErr = iErr * nR / nnR        -- the largest conductance equals bound.
      in combMaybe False (sRHlp nnR nErr (bound - nComb) False st) (ResM nComb)
  | otherwise  = if netSize lResult > bound then Nothing else Just lResult
  where bNet = synthBasic nR iErr isPar
        testCand (vx,n) = case (compare (nR*(1+iErr)) v,compare (nR*(1-iErr)) v) of
                          (EQ,_ ) -> Just n
                          (_ ,EQ) -> Just n
                          (GT,LT) -> Just n
                          (LT,LT) -> Nothing
                          (GT,GT) -> combMaybe isPar rNet nNet
          where v = if isPar then 1/vx else vx
                xRst = nR - v
                (wnR,nnR) = if truncate (xRst*(1+iErr)) /= truncate (xRst*(1-iErr))
                             then (round xRst,0)
                             else properFraction xRst
                nErr = iErr * nR / nnR
                nNet = case (wnR,isPar) of
                        (0,_    ) -> n
                        (_,True ) -> PRes (IntP $ replicate wnR 1,n)
                        (_,False) -> SRes (ResM wnR,n)
                rNet = if nnR <= (iErr * xRst)
                        then Just NilRes
                        else sRHlp (1/nnR) nErr (bound - netSize n - wnR) (not isPar) st'
        rCands1 = parListN bound rdeepseq partNets `seq` take bound partNets
        (rCands2,st') = allResUpTo' st $ min cBoundAll bound
        pCands = nubCand . concat $ rCands2 : rCands1
        pResults = catMaybes $ parMap rdeepseq testCand pCands
        lResult = minimumBy compNet $ bNet : pResults

-- ***
-- functions for generating every resistor network of a given size
-- ***

-- combNets given [(value,ResNet)], make all series/parallel combinations
-- returns a list sorted in increasing value order
combNets        []  = []
combNets ( n   :[]) = [n]
combNets ((v,n):ns) = DM.toAscList serCombsM
  where cnRest = combNets ns
        parCombsM = mapDMNets DM.empty  parComb cnRest
        serCombsM = mapDMNets parCombsM serComb cnRest
        parComb (v2,n2) = (inv $ inv v + inv v2, PRes (n,n2))
        inv x = 1 / x
        serComb (v2,n2) = (v + v2, SRes (n,n2))

-- helper with a map
-- automatically nub this mapping
mapDMNets m f    []  = m
mapDMNets m f (l:ls) = mapDMNets nM f ls
  where (v,n) = f l
        nM = DM.insertWith (flip const) v n m

-- subSelect - given a list of lists, generate all lists resulting from 
--             selecting one element from each of the input lists
subSelect    []  = [[]]
subSelect (l:ls) = concat $ map (flip map (subSelect ls) . (:)) $ nubCand l

-- all of this should be refactored
-- to rid it of its boilerplate
-- but I am lazy
-- so it is not
-- for now

-- allResNets - all resistor networks of size N
-- use maps to combine results, as this is a fast way of merging with uniqueness and sorting
allResNets 0 = []
allResNets 1 = genRes 1
allResNets 2 = genRes 2
allResNets n = restNets
  where nParts = intPartitions n
        rParts = tail nParts
        restNetsLL = parMap rdeepseq (map combNets . subSelect . parMap rdeepseq allResNets) rParts
        genNets = DM.fromList $ genRes n
        restNetsL = genNets : map DM.fromList ( concat restNetsLL )
        restNetsM = foldl1' DM.union restNetsL
        restNets = DM.toAscList restNetsM

-- allResNetsD - all resistor networks of size N, max recursion depth D
allResNetsD 0 n = allResNets n
allResNetsD 1 n = genRes n
allResNetsD d n = restNets
  where nParts = intPartitions n
        rParts = tail nParts
        restNetsLL = parMap rdeepseq (map combNets . subSelect . parMap rdeepseq (allResNetsD $ pred d)) rParts
        genNets = DM.fromList $ genRes n
        restNetsL = genNets : map DM.fromList ( concat restNetsLL )
        restNetsM = foldl1' DM.union restNetsL
        restNets = DM.toAscList restNetsM

-- memoized version, runs slightly faster
-- use maps to combine results, as this is a fast way of merging with uniqueness and sorting
allResNetsM _ 0 = return []
allResNetsM _ 1 = return $ genRes 1
allResNetsM _ 2 = return $ genRes 2
allResNetsM a n = do
    let nParts = intPartitions n
    let rParts = tail nParts
    rNets <- mapM (mapM a) rParts
    let genNets = DM.fromList $ genRes n
    let restNetsLL = concat $ parMap rdeepseq (map combNets . subSelect) rNets
    let restNetsL = genNets : map DM.fromList restNetsLL
    let restNetsM = foldl1' DM.union restNetsL
    return $ DM.toAscList restNetsM

-- memoize function
memoizeFn1' fn st x = runState (tryFn x) st
  where runFn x = do
               y <- fn tryFn x
               m <- get
               put $ DM.insert x y m
               return y
        tryFn x = get >>= \m -> maybe (runFn x) return (DM.lookup x m)

memoizeFn1 fn = fst . memoizeFn1' fn DM.empty 

-- memoize the same function with different arguments, keeping the same memo for all of them
memoizeFnN st fn ls = mFHlp ls [] st
  where mFHlp    []  os st = (os,st)
        mFHlp (l:ls) os st = mFHlp ls (o:os) s
          where (o,s) = memoizeFn1' fn st l

-- from size 1 to X, all networks
allResUpTo :: (RealFrac t, Control.DeepSeq.NFData t) => Int -> [(t,ResNet)]
allResUpTo = fst . allResUpTo' DM.empty

-- allRes with explicitly passed-in and passed-out memo pad
allResUpTo' st x = (DM.toAscList allM,st')
  where (os,st') = memoizeFnN st allResNetsM [1..x]
        allM = foldl' (\m x -> flip DM.union m $ DM.fromList x) DM.empty os
