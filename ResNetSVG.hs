
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

module ResNetSVG where

import ResNetType
import Data.List (intersperse)
import Data.Ratio (numerator,denominator)

-- datatype to pass around when drawing a resistor net
data SVGNetwork = SVGNet
    { sWidth :: Int
    , sHeight :: Int
    , sData :: String
    } deriving (Eq, Show, Read)

-- turn a ResNet into an SVG image
-- assembles SVG from pieces provided by
-- svgHead, netToSVG', and svgTail
netToSVG nn r u = svgHead svgW svgH title ++ path ++ svgTail
  where n = simplifyNet nn
        sNet = netToSVG' n
        path = sData sNet
        nVal = netValue' n
        title = "Rgoal=" ++ (show r) ++ " Ru=" ++ (show u) ++
                " Rnet=" ++ (show $ numerator nVal) ++
                "/" ++ (show $ denominator nVal) ++
                " (" ++ show (fromRational nVal :: Double) ++ ")"
        svgH = show $ sUHeight * sHeight sNet + 2 * sMargin
        svgW = show $ sUWidth * sWidth sNet + 2 * sMargin

-- workhorse for drawing SVGs
-- each clause returns a SVGNetwork datatype
-- whose sData leaves the SVG drawing cursor
-- at the top-right corner of the network
-- this is important because that lets us
-- chain them together pretty easily
netToSVG' NilRes = error "unhandled NilRes: simplify network first" -- NilRes cannot be drawn
netToSVG' (ResM k) = SVGNet k 1 (concat $ replicate k svgRes)       -- a simple series connection
netToSVG' (IntP k) = SVGNet width height nSVG                       -- a parallel connection of
  where lRes = map (netToSVG'.ResM) k                               -- series elements
        height = length k
        width = maximum $ map sWidth lRes
        lSVG = map (normW width) lRes
        vconn = mkConn width 1
        nSVG = (concat $ intersperse vconn lSVG) ++
               " m0,-" ++ show ((height-1) * sUHeight)
netToSVG' (SRes (x,y)) = SVGNet width height nSVG                   -- a series connection of
  where xNet = netToSVG' x                                          -- elements that must first
        yNet = netToSVG' y                                          -- be processed
        width = sWidth xNet + sWidth yNet + 1
        height = max (sHeight xNet) (sHeight yNet)
        wid = quot sUWidth 3
        wdl = sUWidth - 2 * wid
        wd1 = " h" ++ show wid ++ " "
        wd2 = " h" ++ show wdl ++ " "
        nSVG = wd1 ++ sData xNet ++ wd1 ++ sData yNet ++ wd2
netToSVG' (PRes (x,y)) = SVGNet width height nSVG                   -- a parallel connection of
  where xNet = netToSVG' x                                          -- elements that must first
        yNet = netToSVG' y                                          -- be processed
        width = max (sWidth xNet) (sWidth yNet)
        height = sHeight xNet + sHeight yNet
        xD = normW width xNet
        yD = normW width yNet
        conn = mkConn width $ sHeight xNet
        nSVG = xD ++ conn ++ yD ++
               " m0,-" ++ show (sUHeight * sHeight xNet)

-- make connectors for parallel elements given a width
-- and height of elements to connect
mkConn width height = vline ++
               " m-" ++ show (width * sUWidth) ++ ",-" ++
               show (height * sUHeight) ++ vline
  where vline = " v" ++ show (height * sUHeight)

-- normalize the width of an element by adding a wire
-- that elongates the element until it is the specified
-- width
normW width sNet = h1 ++ sData sNet ++ h2
  where wid = sUWidth * (width - sWidth sNet)
        wd2 = quot wid 2
        h1 = if wid == 0 then "" else " h" ++ show wd2 ++ " "
        h2 = if wid == 0 then "" else " h" ++ show (wid - wd2) ++ " "

-- unit sizes, plus margin around the outside of the SVG
sUHeight = 36
sUWidth = 72
sMargin = 10

-- the head of the SVG element specifies width, height, and title
svgHead w h t = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" ++
                w ++ "\" height=\"" ++ h ++ "\" viewbox=\"0 0 " ++ w ++
                " " ++ h ++ "\"><title>" ++ t ++ "</title> \
                \<path style=\"stroke:black;stroke-width=1.5;fill:none\" \
                \d=\"M"++show sMargin++
                (',':(show $ sMargin + div sUHeight 2)) ++ " "

-- the unit element, one resistor drawn as an SVG
svgRes = " h12 l4,12 l8,-24 l8,24 l8,-24 l8,24 l8,-24 l4,12 h12 "

-- end of the SVG string
svgTail = "\"/></svg>"

