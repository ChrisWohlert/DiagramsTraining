
module Main where

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}


import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Size
import Packer
import Diagrams.TwoD.Shapes
import qualified Debug.Trace as D

data Class = Class String deriving (Show)

instance Packable Class where
    packingDims c = (1, 2)


draw :: Class -> Diagram B
draw c = rect 1 2

packedClass = pack (take 10 (repeat (Class "test")))

toDiagram = travelPacked (D.trace "drawing" draw) (|||) (===) (<>) (D.trace (show packedClass) packedClass)

myCircle :: Diagram B
myCircle = circle 3

doubleCalcs = width $ (circle 3 :: D V2 Double)

main = mainWith toDiagram