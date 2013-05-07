
module Main where

import ResNetType
import ResNetSVG
import ResNetSynth
import Network.CGI
import Text.XHtml
import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Data.Ratio (numerator, denominator)
import Text.Printf
import Data.List (intercalate,intersperse)

rsHeader = header . thetitle $ toHtml "N-sized resistor networks"

numResults = 150

showResult res scrN nSize pNum = writePage nInfo scrN
  where resImgs = map pImgNet showNets
        nInfo = paragraph << (show numNets ++ " possible networks." ++ sResults ) +++
                pNumLinks +++
                dlist << resImgs +++
                pNumLinks +++
                paragraph << ("All synthesizable values: " ++
                              ( '[' : (intercalate 
                                       ", " 
                                       ( map (printf "%.5f") $ (map (fromRational . snd) res :: [Double]))) ++ "]" )) +++
                br
        lastResult = let pn1R = (pNum+1)*numResults
                     in if pn1R > numNets then numNets else pn1R
        sResults = if numNets > numResults
                    then " Showing networks " ++ show (1+pNum*numResults) ++ " to " ++ show lastResult ++ "."
                    else ""
        pNumLinks = if numNets > numResults
                     then paragraph << intersperse (toHtml " ") (map showNumAnchor [0..div numNets numResults])
                     else toHtml ""
        showNumAnchor num = if num == pNum
                             then toHtml $ show num
                             else anchor (toHtml $ numS) ! [href hrefS]
          where numS = show num
                hrefS = scrN ++ "?nSize=" ++ (show nSize) ++ "&pNum=" ++ numS
        showNets = take numResults $ drop (numResults * pNum) res
        numNets = length res
        pImgNet (net,value) = [ showValue value , ddef << image ! [src $ scrN ++ "?rDraw=" ++ (show $ simplifyNet net)] ]
        showValue value = dterm << toHtml (
                                    (show $ numerator value) ++ 
                                    "/" ++ 
                                    (show $ denominator value) ++ 
                                    "  (" ++ 
                                    (printf "%.5f" $ (fromRational value :: Double)) ++ 
                                    ")" )

writePage err scrn = err +++
                form ! [method "POST", name "rForm", action scrn] <<
                    [ paragraph << ("Network size: " +++ textfield "nSize" ! [value "0"]),
                      submit "" "Calculate!" ]

cgiMain = do nSizeS <- liftM (fromMaybe "0") $ getInput "nSize"
             pNumS <- liftM (fromMaybe "0") $ getInput "pNum"
             rDraw <- liftM (fromMaybe []) $ getInput "rDraw"
             let nSize = round $ (read nSizeS :: Double)
             let pNum = round $ (read pNumS :: Double)
             scrnQ <- scriptName
             let scrn = takeWhile (/='?') scrnQ
             case (rDraw /= [],nSize < 1,nSize > 11) of
               -- present welcome screen
              (True,_,_) -> do setHeader "Content-Type" "image/svg+xml"
                               let rDrawD = read rDraw
                               output $ netToSVG rDrawD 0 0
              (_,True,_) -> do output $ renderHtml $ rsHeader +++
                                 body << writePage (h3 << "Resistor synthesis") scrn
              (_,_,True) -> do output $ renderHtml $ rsHeader +++
                                 body << writePage (h3 << "Number of networks grows exponentially! You really need n > 11!?") scrn
              _          -> do let resultNets = memoizeFn1 allResNetsM nSize :: [(ResNet,Rational)]
                               output $ renderHtml $ rsHeader +++
                                 body << showResult resultNets scrn nSize pNum

main = do
    runCGI $ handleErrors cgiMain