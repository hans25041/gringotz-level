{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import qualified Data.Aeson as A
import Web.Scotty

import Level


main :: IO ()
main = scotty 3000 $
  get "/:floor" $ do
    floor <- param "floor"
    level <- liftIO $ randMazeL floor
    json level

