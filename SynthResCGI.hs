
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

import ResNetType
import ResNetSVG
import ResNetSynth
import Network.CGI
import Text.XHtml
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
--import qualified Data.ByteString.Char8 as DC8
--import qualified Data.ByteString.Base64 as DBB64
--import qualified Network.URI as NU

rsHeader = header . thetitle $ toHtml "Resistor synthesis"

showResult res scrN rSynth rUnit rPrec = writePage rInfo rS rU rP
  where nVal = rU * netValue res
        nSize = netSize res
        rInfo = paragraph << ("Synthesized value: " +++ show nVal) +++
                paragraph << ("Network size: " +++ show nSize ) +++
                image ! [src $ scrN ++ "?rDraw=" ++ (show $ simplifyNet res)]
        rS = fromRational rSynth :: Double
        rU = fromRational rUnit :: Double
        rP = fromRational rPrec :: Double

writePage err rS rU rP = 
                err +++
                form ! [method "POST", name "rForm"] <<
                    [ paragraph << ("Resistor value: " +++ textfield "rSynth" ! [value rSynth]),
                      paragraph << ("Unit resistor: " +++ textfield "rUnit" ! [value rUnit]),
                      paragraph << ("Precision: " +++ textfield "rPrec" ! [value rPrec]),
                      submit "" "Calculate!" ]
  where rSynth = show rS
        rUnit = show rU
        rPrec = show rP

cgiMain = do rUnitS  <- liftM (fromMaybe "0") $ getInput "rUnit"
             rSynthS <- liftM (fromMaybe "0") $ getInput "rSynth"
             rPrecS  <- liftM (fromMaybe "1e-3") $ getInput "rPrec"
             rDraw   <- liftM (fromMaybe []) $ getInput "rDraw"
             let rUnit  = toRational $ (read rUnitS :: Double)
             let rSynth = toRational $ (read rSynthS :: Double)
             let rPrec  = toRational $ (read rPrecS :: Double)
             case (rDraw /= [],rUnit==0 || rSynth==0,rPrec < 1e-4) of
               -- rendering a resistor
              (True,_,_) -> do setHeader "Content-Type" "image/svg+xml"
                               --let rDrawD = read . DC8.unpack . DBB64.decodeLenient $ DC8.pack rDraw
                               let rDrawD = read rDraw
                               output $ netToSVG rDrawD rSynth rUnit
               -- present welcome screen
              (_,True,_) -> do output $ renderHtml $ rsHeader +++
                                 body << writePage (h3 << "Resistor synthesis") 0 0 1e-3
              (_,_,True) -> do output $ renderHtml $ rsHeader +++
                                 body << writePage (h3 << "C'mon, you don't need better than 0.01%.") (fromRational rSynth :: Double) (fromRational rUnit :: Double) 1e-4
              _          -> do let resultNet = synthRes rSynth rUnit rPrec
                               scrn <- scriptName
                               output $ renderHtml $ rsHeader +++
                                 body << showResult resultNet scrn rSynth rUnit rPrec

main = do
    runCGI $ handleErrors cgiMain
