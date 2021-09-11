{-# LANGUAGE LambdaCase        #-}

module Main where

import           System.Environment
import qualified Data.Vector.Hashtables as H
import qualified Data.HashMap.Strict    as Map

import Lib

main :: IO ()
main =
    getArgs >>= \case
        ("vh": filename:_) -> do
          s <- vh filename
          mapM_ print =<< H.toList s
          -- print =<< H.size s
          return ()
        ("hm": filename:_) -> do
          s <- hm filename
          mapM_ print $ Map.toList s
          -- print $ Map.size s
          return ()
