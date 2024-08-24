module Tile where

import qualified Data.Set as Set
import Data.Function (on)
import Data.List (intersect, intersectBy)
import Data.Foldable (toList)

type Tile a = Either (Set.Set a) a
data Connections a = Connections {north :: a, south :: a, east :: a, west :: a} deriving (Functor, Foldable, Traversable)
instance Applicative Connections where
    pure x = Connections x x x x
    (Connections f0 f1 f2 f3) <*> (Connections x0 x1 x2 x3) = Connections (f0 x0) (f1 x1) (f2 x2) (f3 x3)

class TileContent a where
    pretty :: a -> String
    validators :: Connections (a -> a -> Bool)
    randomOption :: Set.Set a -> Float -> a
    randomOption xs r = Set.elemAt (floor $ r * fromIntegral (Set.size xs)) xs

defaultTile :: Enum a => Tile a
defaultTile = Left $ Set.fromDistinctAscList [toEnum 0 ..]

collapse :: TileContent a => Connections (Maybe (Tile a)) -> Tile a -> Tile a
collapse neighbors (Left set) = packTile $ simplify neighbors set
  where simplify = Set.filter . validate
        validate c x = foldr1 (&&) $ check <$> (validators <*> pure x) <*> c
        check valid = maybe True (either (any valid) valid)
        packTile = fmap headF . guardedE ((==1) . Set.size)
        headF = foldr const (error "head: empty foldable structure")
        guardedE p x = if p x then pure x else Left x
collapse _ val = val

-----

data Pipes = NS | WE | NW | SW | NE | SE | XX | OO deriving (Eq, Enum)

instance TileContent Pipes where

    --pretty :: Pipes -> String
    pretty = pure . \case
        NS -> '║'
        WE -> '═'
        NW -> '╝'
        SW -> '╗'
        NE -> '╚'
        SE -> '╔'
        XX -> '╬'
        OO -> ' '

    -- validators :: Connections (Pipes -> Pipes -> Bool)
    validators = flip on getConnections <$> match
      where
        getConnections :: Pipes -> Connections Bool
        getConnections = \case
            NS -> Connections True True False False 
            WE -> Connections False False True True
            NW -> Connections True False False True
            SW -> Connections False True False True
            NE -> Connections True False True False
            SE -> Connections False True True False
            XX -> Connections True True True True
            OO -> Connections False False False False

        match :: Connections (Connections Bool -> Connections Bool -> Bool)
        match = Connections {east = matchE, west = matchW, south = matchS, north = matchN}
          where
            matchE a b = east a == west b
            matchW a b = west a == east b
            matchN a b = north a == south b
            matchS a b = south a == north b

data Map = Depths | Ocean | Plain | Forest | Mountain | Peak deriving (Eq, Enum)
    
instance TileContent Map where

    --pretty :: Map -> String
    pretty = \case
        Depths   -> "\x1b[044m \x1b[m" 
        Ocean    -> "\x1b[104m \x1b[m" 
        Plain    -> "\x1b[102m \x1b[m" 
        Forest   -> "\x1b[042m \x1b[m" 
        Mountain -> "\x1b[047m \x1b[m" 
        Peak     -> "\x1b[107m \x1b[m" 

    --validators :: Connections (Map -> Map -> Bool)
    validators = pure (match `on` getHeight)
      where
        getHeight :: Map -> Int
        getHeight = fromEnum
        match :: Int -> Int -> Bool
        match a b = abs (a - b) <= 1

data Castle = Tower | HWall | VWall | HGate | Courtyard deriving (Eq, Enum)

instance TileContent Castle where

    --pretty :: Party -> String
    pretty = \case
        Tower -> "[+]"
        HWall -> "==="
        VWall -> "|:|"
        HGate -> "=A="
        Courtyard -> "..."

    -- validators :: Connections (Pipes -> Pipes -> Bool)
    validators = flip on getConnections <$> match
      where
        -- 0: empty, 1: wall socket
        getConnections :: Castle -> Connections [Int]
        getConnections = \case
            Tower -> Connections {north = [0,1], south = [0,1], east = [0,1], west = [0,1]}
            HWall -> Connections {north = [0], south = [0], east = [1], west = [1]}
            VWall -> Connections {north = [1], south = [1], east = [0], west = [0]}
            HGate -> Connections {north = [0], south = [0], east = [1], west = [1]}
            Courtyard -> Connections {north = [0], south = [0], east = [0], west = [0]}

        match :: Connections (Connections [Int] -> Connections [Int] -> Bool)
        match = Connections {east = matchE, west = matchW, south = matchS, north = matchN}
          where
            matchE a b = not $ null $ intersect (east a) (west b)
            matchW a b = not $ null $ intersect (west a) (east b)
            matchN a b = not $ null $ intersect (north a) (south b)
            matchS a b = not $ null $ intersect (south a) (north b)

    --randomOption :: Set.Set a -> Float -> a
    randomOption xs r = go weights'
      where
        (tiles, weights) = unzip $ intersectBy (\(a,_) (b,_) -> a==b) [(Tower, 2), (HWall, 5), (VWall, 5), (HGate, 1), (Courtyard, 10)] (zip (toList xs) [1,1,1,1,1,1,1])
        weights' = zip tiles $ scanl1 (+) $ map (/ (sum weights)) weights
        go ((tile, weight):rest)
          | r < weight = tile
          | otherwise = go rest


