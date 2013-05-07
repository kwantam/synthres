
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

import System.Environment (getArgs)
import System.IO (hPutStrLn,stderr)
import ResNetType
import ResNetSVG
import ResNetSynth

-- quick commandline interface
-- information goes out on stderr, SVG rendition on stdout
main = do
    args <- getArgs
    if length args < 2
     then putStrLn "Usage\n  synthres <unit> <resistance> [precision]"
     else do
        let u = toRational ( read $ args !! 0 :: Double )
        let r = toRational ( read $ args !! 1 :: Double )
        let p = if length args < 3 then 1e-3 else toRational (read $ args !! 2 :: Double)
        let g = synthRes r u p
        let v = netValue g :: Rational
        hPutStrLn stderr $ (show.simplifyNet) g
        hPutStrLn stderr $ (show.netSize) g
        hPutStrLn stderr $ show v
        hPutStrLn stderr $ ("E_rel = "++).show.fromRational $ (r - v*u) / r
        putStrLn $ netToSVG g r u

