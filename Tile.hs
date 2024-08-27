module Tile where

import qualified Data.Set as Set
import Data.Function (on)
import Data.List (unfoldr)
import Data.Tuple (swap)

type Tile a = Either (Set.Set a) a
data Connections a = Connections {north :: a, south :: a, east :: a, west :: a} deriving (Functor, Foldable, Traversable)
instance Applicative Connections where
    pure x = Connections x x x x
    (Connections f0 f1 f2 f3) <*> (Connections x0 x1 x2 x3) = Connections (f0 x0) (f1 x1) (f2 x2) (f3 x3)

class (Bounded a, Enum a) => TileContent a where
    pretty :: a -> String
    validators :: Connections (a -> a -> Bool)
    randomOption :: Set.Set a -> Float -> Tile a
    randomOption xs r = Right $ Set.elemAt (floor $ r * fromIntegral (Set.size xs)) xs

defaultTile :: TileContent a => Tile a
defaultTile = Left $ Set.fromDistinctAscList [minBound .. maxBound]

collapse :: TileContent a => Connections (Maybe (Tile a)) -> Tile a -> Maybe (Tile a)
collapse neighbors (Left set) = packTile $ simplify neighbors set
  where simplify = Set.filter . validate
        validate c x = foldr1 (&&) $ check <$> (validators <*> pure x) <*> c
        check valid = maybe True (either (any valid) valid)
        packTile = fmap getSingleton . guarded (not . null)
        getSingleton x = if (Set.size x == 1) then Right (Set.findMin x) else Left x
        guarded p x = if p x then Just x else Nothing
collapse _ val = Just val

sgr :: Int -> String -> String
sgr n str = "\x1b[" ++ show n ++ "m" ++ str ++ "\x1b[m"

-----

data Pipes = NS | WE | NW | SW | NE | SE | XX | OO deriving (Eq, Bounded, Enum)

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

data Map = Depths | Ocean | Plain | Forest | Mountain | Peak deriving (Eq, Bounded, Enum)
    
instance TileContent Map where

    --pretty :: Map -> String
    pretty = \case
        Depths   -> sgr  44 " "
        Ocean    -> sgr 104 " "
        Plain    -> sgr 102 " "
        Forest   -> sgr  42 " "
        Mountain -> sgr  47 " "
        Peak     -> sgr 107 " "

    --validators :: Connections (Map -> Map -> Bool)
    validators = pure (match `on` getHeight)
      where
        getHeight :: Map -> Int
        getHeight = fromEnum
        match :: Int -> Int -> Bool
        match a b = abs (a - b) <= 1

data Castle = TowerTop | TowerMid | TowerBase | HWall | HWallBase | VWall | VWallEnd | HGate | Courtyard deriving (Eq, Bounded, Enum, Show)

instance TileContent Castle where

    --pretty :: Party -> String
    pretty = \case
        TowerTop -> "[+]"
        TowerMid -> "| |"
        TowerBase -> "|;|"
        HWall -> "zzz"
        HWallBase -> "---"
        HGate -> "-^-"
        VWall -> " N "
        VWallEnd -> "|N|"
        Courtyard -> "   "

    -- validators :: Connections (Pipes -> Pipes -> Bool)
    validators = (\x -> flip elem . x . getConnections) <$> Connections {east = east, west = west, south = south, north = north}
      where
        getConnections :: Castle -> Connections [Castle]
        getConnections = \case
            TowerTop -> Connections {north = [TowerMid, HWall, VWall, Courtyard], south = [TowerMid], east = [HWallBase, Courtyard], west = [HWallBase, Courtyard]}
            TowerMid -> Connections {north = [TowerTop], south = [TowerTop, VWallEnd,TowerBase], east = [HWall,Courtyard], west = [HWall,Courtyard]}
            TowerBase -> Connections {north = [TowerMid], south = [Courtyard], east = [HWallBase, Courtyard], west = [HWallBase, Courtyard]}
            HWall -> Connections {north = [Courtyard], south = [TowerTop, HWallBase, HGate], east = [TowerMid, HWall], west = [TowerMid, HWall]}
            HWallBase -> Connections {north = [HWall], south = [Courtyard], west = [TowerTop, TowerBase, HWallBase, HGate], east = [TowerTop, TowerBase, HWallBase, HGate]}
            HGate -> Connections {north = [HWall], south = [Courtyard], east = [HWallBase], west = [HWallBase]}
            VWall -> Connections {north = [VWallEnd, VWall], south = [TowerTop, VWall], east = [Courtyard], west = [Courtyard]}
            VWallEnd -> Connections {north = [TowerMid], south = [VWall, TowerTop], east = [Courtyard], west = [Courtyard]}
            Courtyard -> Connections {north = [TowerBase, HWallBase, HGate, Courtyard], south = [TowerTop, HWall, Courtyard], east = [TowerTop, TowerMid, TowerBase, VWall, VWallEnd, Courtyard], west = [TowerTop, TowerMid, TowerBase, VWall, VWallEnd, Courtyard]}


    --randomOption :: Set.Set Castle -> Float -> Tile Castle
    randomOption xs r = Right $ find' (r <) $ zip <*> weights $ Set.toList xs
      where
        normalize x = map (/ (sum x)) x
        weights = scanl1 (+) . normalize . map getWeight
        getWeight = \case
            TowerTop -> 1
            TowerMid -> 1
            TowerBase -> 1
            HWall -> 5
            HWallBase -> 5
            HGate -> 3
            VWall -> 5
            VWallEnd -> 5
            Courtyard -> 7
        find' :: (Show a, Show b) => (b -> Bool) -> [(a, b)] -> a
        find' b x = maybe (error $ "not found, r=" ++ show r ++ ", xs=" ++ show xs ++ ", input=" ++ show x) fst . headMay . dropWhile (not . b . snd) $ x
        headMay (x:_) = Just x
        headMay [] = Nothing

data Quad = Quad {nw :: Bool, ne :: Bool, sw :: Bool, se :: Bool} deriving (Eq, Show)

instance Bounded Quad where
    minBound = toEnum 0
    maxBound = toEnum 15

instance Enum Quad where
    toEnum = fromList . unfoldr (Just . swap . fmap toEnum . (`divMod` 2))
        where fromList (a:b:c:d:_) = Quad a b c d
              fromList _ = error "unreachable"
    fromEnum = foldr (\d num -> 2*num + fromEnum d) 0 . (\(Quad a b c d) -> [a,b,c,d])

instance TileContent Quad where

    pretty x = pure $ " ▘▝▀▖▌▞▛▗▚▐▜▄▙▟█" !! (fromEnum x)
    
    validators = Connections {east = matchE, west = matchW, south = matchS, north = matchN}
          where
            matchE a b = ne a == nw b && se a == sw b
            matchW a b = nw a == ne b && sw a == se b
            matchN a b = nw a == sw b && ne a == se b
            matchS a b = sw a == nw b && se a == ne b
