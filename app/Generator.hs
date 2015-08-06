module Main where

import Data.Aeson

import Level

main :: IO ()
main = randMazeL 1 >>= print . encode

