module Main where

import qualified Data.Array.MArray as MA
import qualified Data.Array.IO as IA
import qualified Data.Set as Set
import Data.List (sortBy)
import Data.Function (on)
import qualified System.Random as R
import qualified System.Random.Stateful as RS
import qualified System.Random.Shuffle as Shuffle
import Data.Maybe (catMaybes)
import qualified Data.Time.Clock.POSIX as Time
import Data.Ix (inRange)
import Data.Foldable (toList)

import Tile (Pipes, Map, Castle, Tile, defaultTile, Connections (..), TileContent(..), collapse)

type Coord = (Int, Int)

main :: IO ()
main = do
  grid <- MA.newArray ((0,0), (60,30)) $ (defaultTile :: Tile Pipes)
  seed <- round <$> Time.getPOSIXTime
  gen <- RS.newIOGenM $ R.mkStdGen seed
  loop grid gen []
  (putStrLn . unlines . map (concatMap (either (const " ") pretty))) =<< showGrid grid

loop :: (Eq a, TileContent a, RS.RandomGen g) => IA.IOArray Coord (Tile a) -> RS.IOGenM g -> [Coord] -> IO ()
loop grid gen todo = do
  bounds <- MA.getBounds grid
  case todo of
    [] -> do
      assocs <- MA.getAssocs grid
      case [(a, b) | (a, Left b) <- assocs] of
        []          -> pure ()
        uncollapsed -> do
          let lowestEntropies = takeWhile2 ((==) `on` (Set.size . snd))
                              $ sortBy (compare `on` (Set.size . snd))
                              $ uncollapsed
          (pos, set) <- randElem gen lowestEntropies
          newTile <- (Right . randomOption set) <$> randFloat gen
          --
          MA.writeArray grid pos newTile
          toAdd <- Shuffle.shuffleM $ catMaybes $ toList $ getAdjacents bounds pos
          loop grid gen toAdd

    (pos : rest) -> do
      toProcess <- MA.readArray grid pos
      neighbors <- mapM (traverse (MA.readArray grid)) $ getAdjacents bounds pos
      let processed = collapse neighbors toProcess
      case toProcess == processed of
        True -> loop grid gen rest
        False -> do
          MA.writeArray grid pos processed
          toAdd <- Shuffle.shuffleM $ catMaybes $ toList $ getAdjacents bounds pos
          loop grid gen (rest ++ toAdd)

getAdjacents :: (Coord, Coord) -> Coord -> Connections (Maybe Coord)
getAdjacents bounds pos = guarded (inRange bounds) <$> adjacents pos
  where guarded p x = if p x then Just x else Nothing
        adjacents (x,y) = Connections {east=(x+1, y), west=(x-1, y),
                                       north=(x, y-1), south=(x, y+1)}

takeWhile2 :: (a -> a -> Bool) -> [a] -> [a]
takeWhile2 _ [] = []
takeWhile2 f (x:xs) = x : takeWhile (f x) xs

randElem :: RS.RandomGen g => RS.IOGenM g -> [a] -> IO a
randElem g xs = (xs !!) <$> RS.applyIOGen (R.randomR (0, length xs - 1)) g

randFloat :: RS.RandomGen g => RS.IOGenM g -> IO Float
randFloat g = RS.applyIOGen (R.randomR (0, 1)) g

showGrid :: IA.IOArray Coord a -> IO [[a]]
showGrid grid = do
  ((minx, miny), (maxx, maxy)) <- MA.getBounds grid
  sequence [sequence [MA.readArray grid (x, y) | x <- [minx..maxx]] | y <- [miny..maxy]]
