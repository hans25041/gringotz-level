{-# LANGUAGE FlexibleInstances #-}
module Patch where

import Data.List
import Data.Random
import Data.Random.Extras

import Row
import Tile
import Utilities

data Dimensions = Dimensions Int Int deriving Show
data Patch a    = Patch Dimensions [a]
type PR         = Patch (Row Tile)


instance Show PR where
  show (Patch _ p) = unlines $ map show p

instance Functor Patch where
  fmap f (Patch d l) = Patch d $ fmap f l


emptyP :: Int -> Int -> PR
emptyP w h = Patch (Dimensions w h)
                   $ [wall w]
                  ++ replicate (h-2) (emptyR w)
                  ++ [wall w]


updateP :: (Tile -> Tile) -> Coordinate -> PR -> PR
updateP f c@(Point x y) p@(Patch dims rs) 
  | (not . validPoint p) c = p
  | otherwise = Patch dims $ update (updateR f x) y rs
updateP _ Rand _ = error "Cannot call updateP on Rand"
updateP _ None p = p


validPoint :: PR -> Coordinate -> Bool
validPoint p@(Patch (Dimensions w h) _) c@(Point x y) =
  x > 0 && y > 0 && x < w-1 && y < h-1 && isSpaceP p c
validPoint _ _ = False


isSpaceP :: PR -> Coordinate -> Bool
isSpaceP (Patch _ rs) (Point x y) = isSpaceR (rs !! y) x
isSpaceP _ _ = False


maxXP :: PR -> Int
maxXP (Patch (Dimensions x _) _) = x - 2


randUpdateP :: (Tile -> Tile) -> PR -> IO PR
randUpdateP f p@(Patch (Dimensions w h) _)
  | h <= 2 || w <= 2 = return p
  | otherwise = do
      c <- runRVar (choice $ tsP p Space) StdRandom
      return $ updateP f c p


tsP :: PR -> Tile -> [Coordinate]
tsP (Patch _ rs) t = concat $ zipWith coords indices rSpaces
  where coords y = map (`Point` y)
        indices  = findIndices (const True) rSpaces
        rSpaces  = fmap (tsR t) rs


usP :: PR -> Coordinate
usP p = head $ tsP p UStairs


dsP :: PR -> Coordinate
dsP p = head $ tsP p DStairs


testP :: PR
testP = emptyP 50 20


