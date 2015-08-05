{-# LANGUAGE FlexibleInstances #-}
module Patch where

import Control.Applicative
import Control.Monad.ST
import Data.Array
import Data.List
import Data.Random
import Data.Random.Extras
import System.Random

import Row
import Tile
import Utilities
import Maze

data Dimensions = Dimensions Int Int deriving Show
data Patch a    = Patch Dimensions [a]
type PR         = Patch (Row Tile)


instance Show PR where
  show (Patch _ p) = unlines $ map show p

instance Functor Patch where
  fmap f (Patch d l) = Patch d $ fmap f l


emptyP :: Dimensions -> PR
emptyP d@(Dimensions w h) =
  Patch d $ [wall w]
         ++ replicate (h-2) (emptyR w)
         ++ [wall w]


updateP :: (Tile -> Tile) -> Point -> PR -> PR
updateP f c@(Point x y) p@(Patch dims rs) 
  | (not . validPoint p) c = p
  | otherwise = Patch dims $ update (updateR f x) y rs


validPoint :: PR -> Point -> Bool
validPoint p@(Patch (Dimensions w h) _) c@(Point x y) =
  x > 0 && y > 0 && x < w-1 && y < h-1 && isSpaceP p c


isSpaceP :: PR -> Point -> Bool
isSpaceP (Patch _ rs) (Point x y) = isSpaceR (rs !! y) x


maxXP :: PR -> Int
maxXP (Patch (Dimensions x _) _) = x - 2


maxYP :: PR -> Int
maxYP (Patch (Dimensions _ y) _) = y - 2


randUpdateP :: (Tile -> Tile) -> PR -> IO PR
randUpdateP f p = do
  c <- randPoint p
  return $ updateP f c p


randPoint :: PR -> IO Point
randPoint p = runRVar (choice $ tsP p Space) StdRandom


tsP :: PR -> Tile -> [Point]
tsP (Patch _ rs) t = concat $ zipWith coords indexes rSpaces
  where coords y = map (`Point` y)
        indexes  = findIndices (const True) rSpaces
        rSpaces  = fmap (tsR t) rs


usP :: PR -> Point
usP p = head $ tsP p UStairs


dsP :: PR -> Point
dsP p = head $ tsP p DStairs


testP :: PR
testP = emptyP (Dimensions 50 20)


mazeP :: Dimensions -> IO PR
mazeP (Dimensions w h) = newStdGen >>= stToIO . maze w h >>= return . mazePatch


mazePatch :: Maze -> PR
mazePatch m = Patch dims $ topLine : (concat (map rowLine [0..maxY]))
  where
    topLine = Row $ replicate width Wall
    rowLine y = [rowRLines y, rowBLines y]

    rowRLines y = Row $ concat $
      [Wall] : map rowSeg [0..maxX]
      where rowSeg x = if walls ! (x,y) then [Space, Wall] else [Space, Space]
            walls = rightWalls m


    rowBLines y = Row $ concat $
      (map rowSeg [0..maxX]) ++ [[Wall]]
      where rowSeg x = if walls ! (x,y) then [Wall, Wall] else [Wall, Space]
            walls = belowWalls m


    dims = Dimensions width height

    width  = (1+) $ (2*) $ maxX+1
    height = (2*) $ maxY+1

    maxX = fst $ snd $ bounds $ rightWalls m
    maxY = snd $ snd $ bounds $ rightWalls m


