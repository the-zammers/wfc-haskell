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
import Data.Maybe (catMaybes)
import Data.Ix (inRange)
import Data.Foldable (toList)
import System.Environment (getArgs)

import Tile (Tile, defaultTile, Connections (..), TileContent (..), collapse)
import Tile (Pipes, Map, Castle, Quad)

type Coord = (Int, Int)

main :: IO ()
main = do
    gen <- RS.newIOGenM . R.mkStdGen . round =<< Time.getPOSIXTime
    args <- getArgs
    let tileset = headMay args
    case tileset of
        Just "Pipes"  -> central gen (60,20) $ defaultTile @Pipes
        Just "Map"    -> central gen (60,20) $ defaultTile @Map
        Just "Castle" -> central gen (20,20) $ defaultTile @Castle 
        Just "Quad"   -> central gen (60,20) $ defaultTile @Quad
        _             -> error "Please provide a tileset to use: Pipes, Map, Castle, or Quad"
    where
        headMay (x:_) = Just x
        headMay [] = Nothing
        central gen size tile = do
            grid <- MA.newArray ((0,0), size) tile
            loop grid gen []
            putStr . unlines . map (concatMap (either (const ".") pretty)) =<< showGrid grid

loop :: (Eq a, TileContent a, RS.RandomGen g) => IA.IOArray Coord (Tile a) -> RS.IOGenM g -> [Coord] -> IO ()
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
            let processed = collapse neighbors toProcess
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

showGrid :: IA.IOArray Coord a -> IO [[a]]
showGrid grid = do
    ((minx, miny), (maxx, maxy)) <- MA.getBounds grid
    sequence [sequence [MA.readArray grid (x, y) | x <- [minx..maxx]] | y <- [miny..maxy]]
