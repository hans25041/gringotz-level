{-# LANGUAGE OverloadedStrings #-}
module Level where

import Control.Monad
import Data.Aeson

import Patch
import Tile
import Vault

data Level = Level
    { level :: Int
    , patch :: PR
    }

instance Show Level where
  show (Level l p) = "Level " ++ show l ++ "\n" ++ show p

instance ToJSON Level where
  toJSON (Level l p) = object [ "level" .= l
                              , "patch" .= show p
                              ]



nextL :: Maybe Level -> IO Level
nextL Nothing = randL 1 Nothing Nothing
nextL (Just (Level l p)) = do
  d <- randPoint p
  randL (l + 1) (Just $ dsP p) (Just d)


prevL :: Level -> IO Level
prevL (Level 1 p) = randL 1 Nothing (Just $ usP p)
prevL (Level l p)
  | l < 1     = error "Cannot fetch level less than 1."
  | otherwise = do
      u <- randPoint p
      randL l (Just u) (Just $ usP p)


randL :: Int -> Maybe Point -> Maybe Point -> IO Level
randL i u d = runTransforms (emptyL i) transforms
  where transforms = [ setTile   u UStairs
                     , setTile   d DStairs
                     , randTiles i Monster
                     , randTiles i Gold
                     , randTiles 8 Hole
                     ]


randMazeL :: Int -> IO Level
randMazeL i = do
  l@(Level _ p) <- mazeL i
  u <- randPoint p
  d <- randPoint p
  runTransforms l (transforms (Just u) (Just d))
  where transforms u d = [ setTile   u     UStairs
                         , setTile   d     DStairs
                         , randTiles (i*2) Monster
                         , randTiles 20    Gold
                         ]


addVault :: Level -> Vault -> IO Level
addVault l (Vault (Point x y) (Dimensions w h) (Point dx dy) g ms) =
  runTransforms l transformers
  where transformers =
          [ setTile   (Just $ Point dx dy) VDoor
          , transform (Point x)            [y..(y+h-1)] Wall -- left
          , transform (Point $ x+w-1)      [y..(y+h-1)] Wall -- right
          , transform (flip Point y)       [x..(x+w-1)] Wall -- top
          , transform (flip Point $ y+h-1) [x..(x+w-1)] Wall -- bottom
          , setTiles g  Gold
          , setTiles ms Monster
          ]


runTransforms :: Monad m => a -> [a -> m a] -> m a
runTransforms = foldM (flip ($))


transformRow :: Int -> Tile -> Level -> IO Level
transformRow r t l = transform (flip Point r) (xRangeL l) t l


transformCol :: Int -> Tile -> Level -> IO Level
transformCol c t l = transform (Point c) (yRangeL l) t l


transform :: (Int -> Point) -> [Int] -> Tile -> Level
             -> IO Level
transform f r t l = runTransforms l ts
  where ps = map (Just . f) r
        ts = map (`setTile` t) ps


xRangeL :: Level -> [Int]
xRangeL l = [1..(maxXL l)]


yRangeL :: Level -> [Int]
yRangeL l = [1..(maxYL l)]


maxXL :: Level -> Int
maxXL (Level _ p) = maxXP p


maxYL :: Level -> Int
maxYL (Level _ p) = maxYP p


l1 :: Level
l1 = emptyL 1


mazeL :: Int -> IO Level
mazeL l = mazeP (Dimensions 49 19) >>= return . Level l


emptyL :: Int -> Level
emptyL l = Level l $ emptyP $ Dimensions 49 19


randTiles :: Int -> Tile -> Level -> IO Level
randTiles 0 _ l = return l
randTiles i t l@(Level _ p) = do
  c <- randPoint p
  l' <- setTile (Just c) t l 
  randTiles (i-1) t l'


setTiles :: [Point] -> Tile -> Level -> IO Level
setTiles []     _ l = return l
setTiles (p:ps) t l = setTile (Just p) t l >>= setTiles ps t


setTile :: Maybe Point -> Tile -> Level -> IO Level
setTile Nothing _ l = return l
setTile (Just c) t (Level l p) =
  return $ Level l $ updateP (safeR t) c p



safeR :: Tile -> Tile -> Tile
safeR t Space = t
safeR _ _     = error "Cannot override occupied tile."


