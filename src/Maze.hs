module Maze where

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.STRef
import System.Random


rand :: Random a => (a, a) -> STRef s StdGen -> ST s a
rand range gen = do
    (a, g) <- liftM (randomR range) $ readSTRef gen
    gen `writeSTRef` g
    return a

data Maze = Maze {rightWalls, belowWalls :: Array (Int, Int) Bool}

instance Show Maze where
  show = showMaze
 
maze :: Int -> Int -> StdGen -> ST s Maze
maze width height gen = do
    visited <- mazeArray False
    rWalls  <- mazeArray True
    bWalls  <- mazeArray True
    gen     <- newSTRef gen

    liftM2 (,) (rand (0, maxX) gen) (rand (0, maxY) gen) >>=
        visit gen visited rWalls bWalls

    liftM2 Maze (freeze rWalls) (freeze bWalls)

  where visit gen visited rWalls bWalls here = do
            writeArray visited here True
            let ns = neighbors here
            i <- rand (0, length ns - 1) gen
            forM_ (ns !! i : take i ns ++ drop (i + 1) ns) $ \there -> do
                seen <- readArray visited there
                unless seen $ do
                    removeWall here there
                    visit gen visited rWalls bWalls there
          where removeWall (x1, y1) (x2, y2) = writeArray 
                    (if x1 == x2 then bWalls else rWalls)
                    (min x1 x2, min y1 y2)
                    False
 
        neighbors (x, y) = 
            (if x == 0    then [] else [(x - 1, y    )]) ++
            (if x == maxX then [] else [(x + 1, y    )]) ++
            (if y == 0    then [] else [(x,     y - 1)]) ++
            (if y == maxY then [] else [(x,     y + 1)])
 
        maxX = (div width  2) - 1
        maxY = (div height 2) - 1
 
        mazeArray = newArray ((0, 0), (maxX, maxY))
            :: Bool -> ST s (STArray s (Int, Int) Bool)
 

showMaze :: Maze -> String
showMaze m = concat $ topLine : map showLine [0..maxY]
  where
    topLine = '#' : (concat $ replicate (maxX+1) "##") ++ "\n"
    showLine y = unlines [showRLines y, showBLines y]
    
    showRLines y = concat $
      "#" : map showSeg [0..maxX]
      where showSeg x = if walls ! (x,y) then ".#" else ".."
            walls = rightWalls m
    
    showBLines y = concat $
      map showSeg [0..maxX] ++ ["#"]
      where showSeg x = if walls ! (x,y) then "##" else "#."
            walls = belowWalls m
    
    maxX = fst $ snd $ bounds $ rightWalls m
    maxY = snd $ snd $ bounds $ rightWalls m


main ::  IO ()
main = newStdGen >>= stToIO . maze 25 10 >>= print

