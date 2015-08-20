{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Lib where

import qualified Data.IntSet as ISet
import qualified Data.IntMap.Strict as IMap
import qualified Data.HashMap.Strict as Hash
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad.State.Strict

import UnionFind

assignInt str = do
  (toInt,toWord,!wc) <- get
  case Hash.lookup str toInt of
    Just i  -> return i
    Nothing -> do let wc' = wc+1
                      toInt' = Hash.insert str wc' toInt
                      toWord' = IMap.insert wc' str toWord
                  put (toInt', toWord', wc')
                  return wc'

main = do
  n <- fmap read getLine

  let loop = replicateM n $ do
              ws <- fmap T.words (liftIO T.getLine)
              mapM assignInt ws

  (sets, (toInt, toWord, wc)) <- runStateT loop (Hash.empty, IMap.empty, 0)

  -- run the union find
  uf <- newUnionFind wc
  forM_ sets $ \s -> do
    case s of
      (x:xs) -> forM_ xs $ \y -> update uf x y
      []     -> return ()

  -- 
  roots <- rootsArray uf
  comps <- components wc roots

  forM_ comps $ \cs -> do
    -- emit the set c as words
    T.putStrLn $ T.unwords $ map (\c -> IMap.findWithDefault T.empty c toWord) cs

