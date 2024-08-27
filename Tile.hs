module Tile where

import qualified Data.Set as Set
import Data.Function (on)
import Data.List (find)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)

type Tile a = Either (Set.Set a) a
data Connections a = Connections {north :: a, south :: a, east :: a, west :: a} deriving (Functor, Foldable, Traversable)
instance Applicative Connections where
    pure x = Connections x x x x
    (Connections f0 f1 f2 f3) <*> (Connections x0 x1 x2 x3) = Connections (f0 x0) (f1 x1) (f2 x2) (f3 x3)

class TileContent a where
    pretty :: a -> String
    validators :: Connections (a -> a -> Bool)
    randomOption :: Set.Set a -> Float -> Tile a
    randomOption xs r = Right $ Set.elemAt (floor $ r * fromIntegral (Set.size xs)) xs

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

color :: Int -> String -> String
color n str = "\x1b[" ++ show n ++ "m" ++ str ++ "\x1b[m"

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
        Depths   -> color  44 " "
        Ocean    -> color 104 " "
        Plain    -> color 102 " "
        Forest   -> color  42 " "
        Mountain -> color  47 " "
        Peak     -> color 107 " "

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
        HWall -> "---"
        VWall -> " | "
        HGate -> "-n-"
        Courtyard -> "   "

    -- validators :: Connections (Pipes -> Pipes -> Bool)
    validators = (\x -> flip elem . x . getConnections) <$> Connections {east = east, west = west, south = south, north = north}
      where
        getConnections :: Castle -> Connections [Castle]
        getConnections = \case
            Tower -> Connections {north = [VWall,Courtyard], south = [VWall,Courtyard], east = [HWall,Courtyard], west = [HWall,Courtyard]}
            HWall -> Connections {north = [Courtyard], south = [Courtyard], east = [Tower, HWall, HGate], west = [Tower, HWall, HGate]}
            VWall -> Connections {north = [Tower, VWall], south = [Tower, VWall], east = [Courtyard], west = [Courtyard]}
            HGate -> Connections {north = [Courtyard], south = [Courtyard], east = [HWall], west = [HWall]}
            Courtyard -> Connections {north = [Tower, HWall, HGate, Courtyard], south = [Tower, HWall, HGate, Courtyard], east = [Tower, VWall, Courtyard], west = [Tower, VWall, Courtyard]}


    --randomOption :: Set.Set Castle -> Float -> Tile Castle
    randomOption xs r = Right $ find' (r <) $ zip <*> weights $ toList xs
      where
        normalize x = map (/ (sum x)) x
        weights = scanl1 (+) . normalize . map getWeight
        getWeight = \case
            Tower -> 1
            HWall -> 5
            VWall -> 5
            HGate -> 3
            Courtyard -> 7
        find' :: (b -> Bool) -> [(a, b)] -> a
        find' b = fst . fromMaybe (error "no options") . find (b . snd)

