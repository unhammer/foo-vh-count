{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.HashMap.Strict             as Map
import qualified Data.Text                       as T
import           Data.Text                       (Text)
import           Data.Char                       (toLower)
import           System.IO

import qualified Streamly.Data.Fold                   as FL
import qualified Streamly.Data.Unicode.Stream         as S
import qualified Streamly.FileSystem.Handle           as FH
import qualified Streamly.Prelude                     as S

import qualified Data.Vector.Mutable as V

import qualified Data.Vector.Hashtables as H

type VHT = H.Dictionary (H.PrimState IO) V.MVector Text V.MVector Int


hm :: FilePath -> IO (Map.HashMap Text Int)
hm fp = openFile fp ReadMode >>= go
  where
    go src =
        S.foldl' (flip (Map.alter incf)) mempty (fileToWordStream src)

vh :: FilePath -> IO VHT
vh fp = openFile fp ReadMode >>= go
  where
    go src = do
        t <- H.initialize 628000 :: IO VHT
        S.mapM_ (H.alter t incf) (fileToWordStream src)
        return t

fileToWordStream src =
    S.map (T.pack . map toLower)
         $ S.splitOn (`elem` [' ', '\t', '\n']) FL.toList
         $ S.decodeUtf8Lax
         $ S.unfold FH.read src

incf Nothing  = Just 1
incf (Just x) = Just (x + 1)
{-# INLINE incf #-}
