module Tile where

import qualified Data.Set as Set
import Data.Function (on)

type Tile a = Either (Set.Set a) a
data Connections a = Connections {north :: a, south :: a, east :: a, west :: a} deriving (Functor, Foldable, Traversable)
instance Applicative Connections where
    pure x = Connections x x x x
    (Connections f0 f1 f2 f3) <*> (Connections x0 x1 x2 x3) = Connections (f0 x0) (f1 x1) (f2 x2) (f3 x3)

class TileContent a where
    pretty :: Tile a -> String
    validators :: Connections (a -> a -> Bool)

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

    --pretty :: Tile Pipes -> String
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
