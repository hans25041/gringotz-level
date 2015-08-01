module Tile where

data Coordinate = Point Int Int
                | Rand
                | None deriving (Eq, Show)


data Tile =   Wall
            | Space
            | Hole
            | Gold
            | Monster
            | Player
            | DStairs
            | UStairs
            | VDoor deriving Eq

instance Show Tile where
  show Wall    = "#"
  show Space   = "."
  show Hole    = " "
  show Gold    = "g"
  show Monster = "m"
  show Player  = "@"
  show DStairs = ">"
  show UStairs = "<"
  show VDoor   = "|"


