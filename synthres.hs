
import Data.List

{--

 2: 2 1/2
 3: 3 2/3 1/3
 4: 4 1 3/4 1/2
 5: 5 6/5 4/5 1/5
 6: 6 9/6 8/6 5/6
 7: 7 12/7 10/7 6/7
 8: 8 2 15/8 12/8 7/8
 9: 9 20/9 18/9 14/9 8/9
10: 10 25/10 24/10 21/10 16/10 9/10

n par
n-2 par || 2 ser
n-3 par || 3 ser
n-4 par || 2 ser || 2 ser
n-5 par || 2 ser || 3 ser
n-6 par || 2 ser || 4 ser
n-6 par || 3 ser || 3 ser
n-7 par || 

--}

data ResNet t = NilRes
              | Res t
              | PRes (ResNet t, ResNet t)
              | SRes (ResNet t, ResNet t)
    deriving (Eq, Show)

genPart 1 _ = [[1]]
genPart n m = [n] : mrst
  where rest = zipWith (\x y->[x-y,y]) (repeat n) [m..div n 2]
        mrst = if rest == []
                then []
                else concatMap recFun rest
        recFun (x:y:[]) = zipWith (++) (genPart x y) (repeat [y])

intPartitions = (flip genPart) 1


genRes n = zip gR $ map ((1/).sum.(map ((1/).toRational))) gR
  where gR = intPartitions n

synthBasic iR err isPar rTot rNet
 | 1/iR<err  = (toNetwork $ reverse rNet,rTot)
 | otherwise = synthBasic (1/rR) (err/rR) (not isPar) (rTot+fR)
                $ (if isPar then 1/fR else fR) : rNet
  where fR = toRational $ truncate iR
        rR = (iR-).fromRational $ fR

toNetwork xs = tNHlp xs p
  where tNHlp []     _ = NilRes
        tNHlp (x:xs) p = if p
                          then PRes ( Res x , tNHlp xs False )
                          else SRes ( Res x , tNHlp xs True  )
        p = odd $ length xs

