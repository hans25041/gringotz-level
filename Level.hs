module Level where

import Control.Monad
import Data.Random
import Data.Random.Extras

import Patch
import Tile

data Level = Level Int PR

instance Show Level where
  show (Level l p) = "Level " ++ show l ++ "\n" ++ show p

data Vault = Vault Point Dimensions Point deriving Show
type VaultTransformFunc = Vault -> Int -> Point

type L = PR


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
  where transforms = [ setTile  u UStairs
                     , setTile  d DStairs
                     , randTiles i Monster
                     , randTiles i Gold
                     , randTiles 8 Hole
                     ]


randVault :: Level -> IO Vault
randVault (Level _ p@(Patch (Dimensions w h) _)) = do
  o@(Point x y) <- randPoint p
  w' <- runRVar (choice [3..(w-x)]) StdRandom
  h' <- runRVar (choice [3..(h-y)]) StdRandom
  dp <- placeDoor o (Dimensions w' h')
  return $ Vault o (Dimensions w' h') dp

  where placeDoor (Point x y) (Dimensions w' h') = do
          let sr 
                 | x < 3          = [x+w'-1] 
                 | w-(x+w'-1) < 3 = [x]
                 | otherwise      = [x,x+w'-1]
          dx <- runRVar (choice sr) StdRandom
          dy <- runRVar (choice [(y+1)..(y+h'-2)]) StdRandom
          return $ Point dx dy


addVault :: Level -> Vault -> IO Level
addVault l (Vault (Point x y) (Dimensions w h) (Point dx dy)) =
  runTransforms l transformers
  where transformers =
          [ setTile   (Just $ Point dx dy) VDoor
          , transform (Point x)            [y..(y+h-1)] Wall -- left
          , transform (Point $ x+w-1)      [y..(y+h-1)] Wall -- right
          , transform (flip Point y)       [x..(x+w-1)] Wall -- top
          , transform (flip Point $ y+h-1) [x..(x+w-1)] Wall -- bottom
          ]


v1 :: Vault
v1 = Vault (Point 2 4) (Dimensions 5 8) (Point 6 6)


runTransforms :: Monad m => a -> [a -> m a] -> m a
runTransforms = foldM (flip ($))


transformRow :: Level -> Int -> Tile -> IO Level
transformRow l r t = transform (flip Point r) (xRangeL l) t l

transform :: (Int -> Point) -> [Int] -> Tile -> Level
             -> IO Level
transform f r t l = runTransforms l ts
  where ps = map (Just . f) r
        ts = map (`setTile` t) ps


xRangeL :: Level -> [Int]
xRangeL l = [1..(maxXL l)]

maxXL :: Level -> Int
maxXL (Level _ p) = maxXP p


l1 :: Level
l1 = emptyL 1


emptyL :: Int -> Level
emptyL l = Level l (emptyP 50 20)


randTiles :: Int -> Tile -> Level -> IO Level
randTiles 0 _ l = return l
randTiles i t l@(Level _ p) = do
  c <- randPoint p
  l' <- setTile (Just c) t l 
  randTiles (i-1) t l'


setTile :: Maybe Point -> Tile -> Level -> IO Level

setTile (Just c) t (Level l p) =
  return $ Level l $ updateP (safeR t) c p

setTile Nothing _ l = return l


safeR :: Tile -> Tile -> Tile
safeR t Space = t
safeR _ _     = error "Cannot override occupied tile."


