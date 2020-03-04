module Packer ( Packed(..)
              , Packable(packingDims)
              , pack
              , travelPacked
              ) where

import Data.Either
import Data.List
import qualified Debug.Trace as D

type East a = Packed a
type South a = Packed a

data Packed a = Packed (Maybe (East a)) (Maybe (South a)) a deriving (Show, Eq)

class Packable a where
    packingDims :: a -> (Double, Double)

makePacked :: (Packable a) => a -> Packed a
makePacked m = Packed Nothing Nothing m

pack :: Packable a => [a] -> Packed a
pack packables = 
    let 
        (p:ps) = map makePacked $ reverse $ sortOn (\ a -> let (x, y) = packingDims a in x * y) packables
    in
        pack' p ps

pack' p [] = p
pack' c@(Packed east south m) (p:ps) = 
    let
        allEast = easts c
        width = absoluteWidth c
        (height, index) = minimum $ map (\ (e, i) -> (absoluteHeight e, i)) $ map (\ (e, i) -> D.trace (showDepth e) (e, i)) (zip (c:allEast) [0..])
        goEast = D.trace (show (width, height, index)) $ width < height
    in
        pack' (if goEast then packEast c p else fLevel index packSouth c p) ps


showDepth :: (Packable a) => Packed a -> String
showDepth = show . length . souths

fLevel :: Int -> (Packed a -> Packed a -> Packed a) -> Packed a -> Packed a -> Packed a
fLevel l f p n = fLevel' l f p n 0

fLevel' l f p@(Packed (Just east) south m) n t = if l == t then f p n else packEastDirect p (fLevel' l f east n (t + 1))
fLevel' l f p@(Packed (Nothing) south m) n t = f p n


packSouth :: Packed a -> Packed a -> Packed a
packSouth (Packed east (Just south) m) new = Packed east (Just $ packSouth south new) m
packSouth (Packed east Nothing m) new = Packed east (Just new) m

packEast :: Packed a -> Packed a -> Packed a
packEast (Packed (Just east) south m) new = Packed (Just $ packEast east new) south m
packEast (Packed Nothing south m) new = Packed (Just new) south m

packEastDirect (Packed _ south m) new = Packed (Just new) south m

absoluteWidth :: (Packable a) => Packed a -> Double
absoluteWidth p@(Packed (Nothing) _ _) = getWidth p
absoluteWidth p@(Packed (Just east) _ _) = getWidth p + (absoluteWidth east)

absoluteHeight :: (Packable a) => Packed a -> Double
absoluteHeight p@(Packed east (Nothing) _) = getHeight p
absoluteHeight p@(Packed _ (Just south) _) = getHeight p + (absoluteHeight south)

getWidth :: (Packable a) => Packed a -> Double
getWidth = fst . getSize

getHeight :: (Packable a) => Packed a -> Double
getHeight = snd . getSize

getSize p = 
    case getPacked p of
        (Just m) -> packingDims m
        Nothing -> (0, 0)

easts :: Packable a => Packed a -> [Packed a]
easts (Packed (Just east) south m) = east : (easts east)
easts (Packed (Nothing) south m) = []

souths :: Packable a => Packed a -> [Packed a]
souths (Packed east (Just south) m) = south : (souths south)
souths (Packed east (Nothing) m) = []

getPacked (Packed _ _ m) = Just m

travelPacked :: (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b -> b) -> Packed a -> b
travelPacked f fe fs mo p@(Packed (Just e) (Just s) m) = mo (fe (f m) (travelPacked f fe fs mo e)) (fs (f m) (travelPacked f fe fs mo s))
travelPacked f fe fs mo p@(Packed Nothing (Just s) m) = (fs (f m) (travelPacked f fe fs mo s))
travelPacked f fe fs mo p@(Packed (Just e) Nothing m) = (fe (f m) (travelPacked f fe fs mo e))
travelPacked f fe fs mo p@(Packed Nothing Nothing m) = f m