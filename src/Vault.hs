module Vault where

import Control.Monad
import Data.Random
import Data.Random.Extras

import Patch
import Tile


data Vault = Vault 
    { origin   :: Point
    , size     :: Dimensions
    , door     :: Point
    , gold     :: [Point]
    , monsters :: [Point]
    } deriving Show


v1 :: Vault
v1 = Vault { origin   = Point 2 4
           , size     = Dimensions 5 8
           , door     = Point 6 6
           , gold     = [Point 5 5]
           , monsters = [Point 5 6]
           }


randVault :: PR -> Int -> Int -> IO Vault
randVault p@(Patch (Dimensions w h) _) gc mc = do
  o@(Point x y) <- randPoint p
  w' <- runRVar (choice [3..(w-x)]) StdRandom
  h' <- runRVar (choice [3..(h-y)]) StdRandom
  let dims = Dimensions w' h'
  dp <- randEdge o dims
  gs <- replicateM gc (randSpace o dims)
  ms <- replicateM mc (randSpace o dims)
  return $ Vault o dims dp gs ms

  where randEdge (Point x y) (Dimensions w' h') = do
          let sr 
                 | x < 3          = [x+w'-1] 
                 | w-(x+w'-1) < 3 = [x]
                 | otherwise      = [x,x+w'-1]
          dx <- runRVar (choice sr) StdRandom
          dy <- runRVar (choice [(y+1)..(y+h'-2)]) StdRandom
          return $ Point dx dy

        randSpace (Point x y) (Dimensions w' h') = do
          dx <- runRVar (choice [(x+1)..(x+w'-1)]) StdRandom
          dy <- runRVar (choice [(y+1)..(y+h'-1)]) StdRandom
          return $ Point dx dy


