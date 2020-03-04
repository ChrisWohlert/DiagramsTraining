
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
    packingDims (Class c) = (3, fromIntegral $ length c)


draw :: Class -> Diagram B
draw c = let (x,y) = packingDims c in rect x y # alignT

packedClass = pack (take 10 (map Class (["test", "ds", "a", "dsa", "dwad", "dsa", "ds", "1"])))

toDiagram = travelPacked (D.trace "drawing" draw) (|||) (===) (<>) (D.trace (show packedClass) packedClass)

myCircle :: Diagram B
myCircle = circle 3

doubleCalcs = width $ (circle 3 :: D V2 Double)

main = mainWith toDiagram