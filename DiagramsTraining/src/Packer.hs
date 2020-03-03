module Packer ( Packed
              , Packable 
              ) where

type East a = Packed a
type South a = Packed a

data CantPack = NoSpace | Occupied

data Packed a = Packed (Maybe (East a)) (Maybe (South a)) a | NoPack

class Packable a where
    packingDims :: a -> (Double, Double)


pack :: Packable a => [a] -> Packed a
pack (p:ps) = undefined

pack' root (p:nextP:ps) = 
    let
        e = packEast p nextP
        s = packSouth p nextP
    in
        

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