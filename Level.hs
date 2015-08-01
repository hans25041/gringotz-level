module Level where

import Control.Applicative
import Control.Monad

import Patch
import Tile

data Level = Level Int PR

instance Show Level where
  show (Level l p) = "Level " ++ show l ++ "\n" ++ show p

data Vault = Vault (Int, Int) (Int, Int) (Int, Int) deriving Show
type VaultTransformFunc = Vault -> Int -> Coordinate

type L = PR


nextL :: Maybe Level -> IO Level
nextL Nothing = randL 1 None Rand
nextL (Just (Level l p)) =
  randL (l + 1) (head $ tsP p DStairs) Rand


prevL :: Level -> IO Level
prevL (Level 1 p) = randL 1 None (usP p)
prevL (Level l p)
  | l < 1     = error "Cannot fetch level less than 1."
  | otherwise = randL l Rand (usP p)


randL :: Int -> Coordinate -> Coordinate -> IO Level
randL i u d = runTransforms (emptyL i) transforms
  where transforms = [ setTile  u UStairs
                     , setTile  d DStairs
                     , setTiles i Monster
                     , setTiles i Gold
                     , setTiles 8 Hole
                     ]


addVault :: Level -> Vault -> IO Level
addVault l (Vault (x, y) (w, h) (dx, dy)) = runTransforms l transformers
  where transformers =
          [ setTile   (Point dx dy) VDoor
          , transform (Point x)          [x..(x+w)] Wall
          , transform (Point $ x+w)      [x..(x+w)] Wall
          , transform (flip Point y)     [y..(y+h)] Wall
          , transform (flip Point $ y+h) [y..(y+h)] Wall
          ]


v1 :: Vault
v1 = Vault (5, 5) (5, 5) (5, 8)


runTransforms :: Monad m => a -> [a -> m a] -> m a
runTransforms = foldM (flip ($))


transformRow :: Level -> Int -> Tile -> IO Level
transformRow l r t = transform (flip Point r) (xRangeL l) t l

transform :: (Int -> Coordinate) -> [Int] -> Tile -> Level
             -> IO Level
transform f r t l = runTransforms l ts
  where ps = map f r
        ts = map (`setTile` t) ps


xRangeL :: Level -> [Int]
xRangeL l = [1..(maxXL l)]

maxXL :: Level -> Int
maxXL (Level _ p) = maxXP p


l1 :: Level
l1 = emptyL 1


emptyL :: Int -> Level
emptyL l = Level l (emptyP 50 20)


setTiles :: Int -> Tile -> Level -> IO Level
setTiles 0 _ l = return l
setTiles i t l = setTile Rand t l >>= setTiles (i-1) t


setTile :: Coordinate -> Tile -> Level -> IO Level

setTile c@(Point _ _) t (Level l p) =
  return $ Level l $ updateP (safeR t) c p

setTile Rand t (Level l p) =
  Level l <$> randUpdateP (safeR t) p

setTile None _ l = return l


safeR :: Tile -> Tile -> Tile
safeR t Space = t
safeR _ _     = error "Cannot override occupied tile."


