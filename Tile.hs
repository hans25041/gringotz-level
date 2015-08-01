module Tile where

data Point = Point Int Int deriving Show

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


