{-# LANGUAGE FlexibleInstances #-}
module Row where

import Control.Applicative
import Data.List
import System.Random

import Tile
import Utilities


data Row a = Row [a]
type R = Row Tile

instance Show R where
  show (Row r) = concat $ show <$> r

instance Functor Row where
  fmap f (Row l) = Row $ fmap f l


wall :: Int -> R
wall w = Row $ replicate w Wall


emptyR :: Int -> R
emptyR w = Row $ [Wall] 
              ++ replicate (w-2) Space
              ++ [Wall]


tsR :: Tile -> R -> [Int]
tsR t (Row ts) = elemIndices t ts


isSpaceR :: R -> Int -> Bool
isSpaceR (Row ts) i = ts !! i == Space


spacesR :: R -> [Int]
spacesR (Row ts) = elemIndices Space ts


downR :: R -> [Int]
downR (Row ts) = elemIndices DStairs ts


updateR :: (Tile -> Tile) -> Int -> R -> R
updateR f i (Row ts) = Row $ update f i ts


randUpdateR :: (Tile -> Tile) -> R -> IO R
randUpdateR f r@(Row ts)
  | length ts <= 2 = return r
  | otherwise      = do
      i <- randomRIO (1, length ts-2)
      return $ updateR f i r



