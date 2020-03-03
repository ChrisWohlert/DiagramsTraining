
module Main where

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}


import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Size

myCircle :: Diagram B
myCircle = circle 3

doubleCalcs = size2D $ (circle 3 :: D V2 Double)

main = print doubleCalcs