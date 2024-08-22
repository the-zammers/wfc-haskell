module Tile where

import qualified Data.Set as Set
import Data.Function (on)

type Tile a = Either (Set.Set a) a
data Connections a = Connections {north :: a, south :: a, east :: a, west :: a} deriving (Functor, Foldable, Traversable)

class TileContent a where
    pretty :: Tile a -> String
    valid :: Connections (Maybe (Tile a)) -> a -> Bool

defaultTile :: Enum a => Tile a
defaultTile = Left $ Set.fromDistinctAscList [toEnum 0 ..]

collapse :: TileContent a => Connections (Maybe (Tile a)) -> Tile a -> Tile a
collapse neighbors (Left set) = packTile $ simplify neighbors set
  where simplify = Set.filter . valid
        packTile = fmap headF . guardedE ((==1) . Set.size)
        headF = foldr const (error "head: empty foldable structure")
        guardedE p x = if p x then pure x else Left x
collapse _ val = val

-----

data Pipes = NS | WE | NW | SW | NE | SE | XX | OO deriving (Eq, Enum)

instance TileContent Pipes where

    --pretty :: Tile -> String
    pretty = pure . either (const ' ') showPipes
        where showPipes = \case
                NS -> '║'
                WE -> '═'
                NW -> '╝'
                SW -> '╗'
                NE -> '╚'
                SE -> '╔'
                XX -> '╬'
                OO -> ' '

    --valid :: Connections (Maybe (Tile Pipes)) -> Pipes -> Bool
    valid c x = (check matchE $ east c) && (check matchW $ west c) && (check matchN $ north c) && (check matchS $ south c)
      where
        check :: (Connections Bool -> Connections Bool -> Bool) -> Maybe (Tile Pipes) -> Bool
        check f = \case
            Nothing -> True
            Just (Right y) -> (f `on` getConnections) x y
            Just (Left y) -> any ((f `on` getConnections) x) y

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

        matchE, matchW, matchN, matchS :: Connections Bool -> Connections Bool -> Bool
        matchE a b = east a == west b
        matchW a b = west a == east b
        matchN a b = north a == south b
        matchS a b = south a == north b

