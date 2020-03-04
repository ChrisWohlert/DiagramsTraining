module Packer ( Packed
              , Packable 
              ) where

import Data.Either
import Data.List

type East a = Packed a
type South a = Packed a

data CantPack = NoSpace | Occupied

data Packed a = Packed (Maybe (East a)) (Maybe (South a)) a | NoPack

class Packable a where
    packingDims :: a -> (Double, Double)

makePacked :: (Packable a) => a -> Packed a
makePacked m = Packed Nothing Nothing m

pack :: Packable a => [a] -> Packed a
pack (p:ps) = pack' (makePacked p) (map makePacked ps)

pack' (Packed east south m) (p:ps) = 
    let
        easts = rights $ map (packEast p) ps
        souths = rights $ map (packSouth p) ps
        newEast = head $ sortOn fst $ map (\ p -> (absoluteWidth p * absoluteHeight p, p)) $ map (\ e -> Packed (Just e) south m) easts
        newSouth = head $ sortOn fst $ map (\ p -> (absoluteWidth p * absoluteHeight p, p)) $ map (\ s -> Packed east (Just s) m) souths
    in
        if fst newEast >= fst newSouth then Packed (Just $ snd newEast) south m else Packed east (Just $ snd newSouth) m


packSouth :: Packed a -> Packed a -> Either CantPack (Packed a)
packSouth (Packed east Nothing m) new = Right $ Packed east (Just new) m
packSouth NoPack _ = Left NoSpace
packSouth _ new = Left Occupied

packEast :: Packed a -> Packed a -> Either CantPack (Packed a)
packEast (Packed Nothing south m) new = Right $ Packed (Just new) south m
packEast NoPack _ = Left NoSpace
packEast _ new = Left Occupied

absoluteWidth :: (Packable a) => Packed a -> Double
absoluteWidth NoPack = 0
absoluteWidth p@(Packed (Nothing) _ _) = getWidth p
absoluteWidth p@(Packed (Just east) _ _) = getWidth p + (absoluteWidth east)

absoluteHeight :: (Packable a) => Packed a -> Double
absoluteHeight NoPack = 0
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
easts (NoPack) = []

souths :: Packable a => Packed a -> [Packed a]
souths (Packed east (Just south) m) = south : (souths south)
souths (Packed east (Nothing) m) = []
souths (NoPack) = []

getPacked (Packed _ _ m) = Just m
getPacked NoPack = Nothing