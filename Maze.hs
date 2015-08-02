module Maze where

import Level
import Patch
import Tile

grid :: Level -> Point -> Point -> IO Level
grid (Level _ (Patch (Dimensions w h) _)) d u =
  runTransforms l1 transforms
  where transforms =
          map (`transformRow` Wall) (filter even [1..h]) ++
          map (`transformCol` Wall) (filter even [1..w]) ++
          [ setTile (Just d) DStairs
          , setTile (Just u) UStairs
          ]

