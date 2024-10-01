{-# LANGUAGE TypeApplications, MultiWayIf #-}

module Main where

import qualified Data.Array.MArray as MA
import qualified Data.Array.IO as IA
import qualified Data.Set as Set

import qualified System.Random as R
import qualified System.Random.Stateful as RS
import qualified System.Random.Shuffle as Shuffle
import qualified Data.Time.Clock.POSIX as Time

import Data.List (minimumBy)
import Data.Function (on)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Ix (inRange)
import Data.Foldable (toList)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Tile (Tile, defaultTile, Connections (..), TileContent (..), collapse)
import Tile (Pipes, Map, Castle, Quad, Gradient, MapTwo, Mandarinish)

type Coord = (Int, Int)

main :: IO ()
main = do
    args <- getArgs
    let tileset = args !? 0
    let width = fromMaybe 60 (readMaybe @Int =<< args !? 1) - 1
    let height = fromMaybe 20 (readMaybe @Int =<< args !? 2) - 1

    gen <- RS.newIOGenM . R.mkStdGen . round =<< Time.getPOSIXTime
    let base tile = do
            grid <- MA.newArray ((0,0), (width, height)) tile
            loop grid gen []
            putStr . showGrid =<< extractGrid grid
    case tileset of
        Just "Pipes"       -> base $ defaultTile @Pipes
        Just "Map"         -> base $ defaultTile @Map
        Just "Castle"      -> base $ defaultTile @Castle 
        Just "Quad"        -> base $ defaultTile @Quad
        Just "Gradient"    -> base $ defaultTile @Gradient
        Just "MapTwo"      -> base $ defaultTile @MapTwo
        Just "Mandarinish" -> base $ defaultTile @Mandarinish
        _ -> error "Please provide a tileset to use: Pipes, Map, MapTwo, Castle, Quad, Gradient, or Mandarinish"

loop :: (TileContent a, RS.RandomGen g) => IA.IOArray Coord (Tile a) -> RS.IOGenM g -> [Coord] -> IO ()
loop grid gen todo = do
    bounds <- MA.getBounds grid
    case todo of
        [] -> do
            assocs <- MA.getAssocs grid
            case [(a, b) | (a, Left b) <- assocs] of
                []          -> pure ()
                uncollapsed -> do
                    let lowestEntropy = minimumBy (compare `on` (Set.size . snd)) $ uncollapsed
                    let lowestEntropies = filter (((==) `on` (Set.size . snd)) lowestEntropy) uncollapsed
                    (pos, set) <- randElem gen lowestEntropies
                    MA.writeArray grid pos . randomOption set =<< randFloat gen
                    toAdd <- Shuffle.shuffleM $ catMaybes $ toList $ getAdjacents bounds pos
                    loop grid gen toAdd

        (pos : rest) -> do
            toProcess <- MA.readArray grid pos
            neighbors <- mapM (traverse (MA.readArray grid)) $ getAdjacents bounds pos
            case collapse neighbors toProcess of
                Nothing -> error "Unable to resolve! Possible issue: asymmetrical validators"
                Just processed ->
                    if | toProcess == processed -> loop grid gen rest
                       | otherwise -> do
                            MA.writeArray grid pos processed
                            toAdd <- Shuffle.shuffleM $ catMaybes $ toList $ getAdjacents bounds pos
                            loop grid gen (rest ++ toAdd)

getAdjacents :: (Coord, Coord) -> Coord -> Connections (Maybe Coord)
getAdjacents bounds = fmap (guarded (inRange bounds)) . adjacents
    where adjacents (x,y) = Connections {east=(x+1, y), west=(x-1, y), north=(x, y-1), south=(x, y+1)}

guarded :: (a -> Bool) -> a -> Maybe a
guarded p x = if p x then Just x else Nothing

randElem :: RS.RandomGen g => RS.IOGenM g -> [a] -> IO a
randElem g xs = (xs !!) <$> RS.applyIOGen (R.randomR (0, length xs - 1)) g

randFloat :: RS.RandomGen g => RS.IOGenM g -> IO Float
randFloat g = RS.applyIOGen (R.randomR (0, 1)) g

extractGrid :: IA.IOArray Coord a -> IO [[a]]
extractGrid grid = do
    ((minx, miny), (maxx, maxy)) <- MA.getBounds grid
    sequence [sequence [MA.readArray grid (x, y) | x <- [minx..maxx]] | y <- [miny..maxy]]

showGrid :: TileContent a => [[Tile a]] -> String
showGrid = unlines . map (concatMap (either (const ".") pretty))

(!?) :: [a] -> Int -> Maybe a
{-# INLINABLE (!?) #-}
xs !? n
    | n < 0     = Nothing
    | otherwise = foldr (\x r k -> case k of
        0 -> Just x
        _ -> r (k-1)) (const Nothing) xs n
