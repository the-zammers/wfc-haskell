module Tile where

import qualified Data.Set as Set
import Data.Function (on)

data Shape = NS | WE | NW | SW | NE | SE | XX | OO deriving (Eq, Enum)
type Tile = Either (Set.Set Shape) Shape
data Connections a = Connections {north :: a, south :: a, east :: a, west :: a} deriving (Functor, Foldable, Traversable)

defaultTile :: Tile
defaultTile = Left $ Set.fromDistinctAscList [toEnum 0 ..]

showShape :: Shape -> Char
showShape = \case
  NS -> '║'
  WE -> '═'
  NW -> '╝'
  SW -> '╗'
  NE -> '╚'
  SE -> '╔'
  XX -> '╬'
  OO -> ' '

pretty :: Tile -> Char
pretty = either (const ' ') showShape

getConnections :: Shape -> Connections Bool
getConnections = \case
  NS -> Connections True True False False 
  WE -> Connections False False True True
  NW -> Connections True False False True
  SW -> Connections False True False True
  NE -> Connections True False True False
  SE -> Connections False True True False
  XX -> Connections True True True True
  OO -> Connections False False False False

collapse :: Connections (Maybe Tile) -> Tile -> Tile
collapse neighbors (Left set) = packTile $ simplify neighbors set
  where simplify = Set.filter . valid
        packTile = fmap headF . guardedE ((==1) . Set.size)
        headF = foldr const (error "head: empty foldable structure")
        guardedE p x = if p x then pure x else Left x
collapse _ val = val

valid :: Connections (Maybe Tile) -> Shape -> Bool
valid c x = (check matchE $ east c) && (check matchW $ west c) && (check matchN $ north c) && (check matchS $ south c)
  where
    check f = \case
      Nothing -> True
      Just (Right y) -> (f `on` getConnections) x y
      Just (Left y) -> any ((f `on` getConnections) x) y

matchE, matchW, matchN, matchS :: Connections Bool -> Connections Bool -> Bool
matchE x y = east x == west y
matchW x y = west x == east y
matchN x y = north x == south y
matchS x y = south x == north y
