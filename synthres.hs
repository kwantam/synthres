
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

genPart 1 _ = [[1]]
genPart n m = [n] : mrst
  where rest = zipWith (\x y->[x-y,y]) (repeat n) [m..div n 2]
        mrst = if rest == []
                then []
                else concatMap recFun rest
        recFun (x:y:[]) = zipWith (++) (genPart x y) (repeat [y])

intPartitions = (flip genPart) 1


genRes n = zip rNets rVals
  where gR = intPartitions n
        rNets = map (IntP.(map toRational)) gR
        rVals = map ((1/).sum.(map ((1/).toRational))) gR

synthBasic iR err isPar
 | iR == 0   = NilRes
 | otherwise = rCons (nNet, rNet)
  where fR = toRational $ truncate iR
        rR = iR - fR
        nErr = if isPar            -- compute next error bounds
                then iR / rR / err
                else rR / iR / err
        nNet = case (fR,isPar) of
                (0,_    ) -> NilRes
                (_,True ) -> IntP $ take (round fR) $ repeat 1
                (_,False) -> ResM fR
        rCons = if isPar then PRes else SRes
        rNet = if rR == 0 || ( isPar && rR >= err) || ( (not isPar) && rR <= err )
                then NilRes
                else synthBasic (1/rR) nErr (not isPar)

toNetwork xs = tNHlp xs False
  where tNHlp []     _ = NilRes
        tNHlp (x:xs) p = if p
                          then PRes ( parRes x , tNHlp xs False )
                          else SRes ( ResM x , tNHlp xs True  )
        parRes x = if x == 0 then NilRes else IntP $ take (round $ 1/x) $ repeat 1

netSize NilRes = 0
netSize (ResM k) = round k
netSize (IntP k) = round $ sum k
netSize (PRes (x,y)) = netSize x + netSize y
netSize (SRes (x,y)) = netSize x + netSize y

compNet n1 n2 = compare (netSize n1) (netSize n2)

combNet isPar n1 n2
 | isPar     = PRes ( n1 , n2 )
 | otherwise = SRes ( n1 , n2 )

combMaybe isPar (Just n1,(n2,v2)) = Just $ combNet isPar n1 n2
combMaybe _     (Nothing,_)       = Nothing

synthRes r unit err = if isNothing hlpRes
                       then bNet
                       else fromJust hlpRes
   where rNorm = r / unit
         bNet = synthBasic rNorm err False
         bound = netSize bNet
         hlpRes = sRHlp rNorm err bound False

sRHlp nR iErr bound isPar
  | bound <= 0 = Nothing
  | nR == 0    = Just NilRes
  | otherwise  = if netSize lResult > bound then Nothing else Just lResult
  where bNet = synthBasic nR iErr isPar
        testCand (n,v) = case (compare nR v) of
                          (EQ) -> Just n
                          (LT) -> Nothing
                          (GT) -> combMaybe isPar (rNet,(nNet,undefined))
          where xRst = nR - v
                wnR = toRational $ truncate xRst
                nnR = xRst - wnR
                nErr = if isPar
                        then nR / nnR / iErr
                        else nnR / nR / iErr
                nNet = case (wnR,isPar) of
                        (0,_    ) -> n
                        (_,True ) -> PRes (IntP $ take (round wnR) $ repeat 1,n)
                        (_,False) -> SRes (ResM wnR,n)
                rNet = if nnR == 0 || ( isPar && nnR >= iErr ) || ( (not isPar) && nnR <= iErr )
                        then Just NilRes
                        else sRHlp (1/nnR) nErr (bound - netSize n) (not isPar)
        pCands = concatMap genRes [1..bound]
        pResults = catMaybes $ parMap rdeepseq testCand pCands
        lResult = minimumBy compNet $ bNet : pResults

