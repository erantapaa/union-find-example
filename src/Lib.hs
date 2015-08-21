{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Lib where

import qualified Data.IntSet as ISet
import qualified Data.IntMap.Strict as IMap
import qualified Data.HashMap.Strict as Hash
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad.State.Strict

import qualified UnionFindVector   as UF64
import qualified UnionFindVector32 as UF32

import System.IO

initWarn = hSetBuffering stderr NoBuffering
warn str = hPutStrLn stderr str

assignInt str = do
  (toInt,toWord,!wc) <- get
  case Hash.lookup str toInt of
    Just i  -> return i
    Nothing -> do let wc' = wc+1
                      toInt' = Hash.insert str wc' toInt
                      toWord' = IMap.insert wc' str toWord
                  put (toInt', toWord', wc')
                  return wc'

readSets = do
  n <- fmap read getLine
  let loop = replicateM n $ do
              ws <- fmap T.words (liftIO T.getLine)
              mapM assignInt ws
  runStateT loop (Hash.empty, IMap.empty, 0)

emitSet toWord cs = 
  T.putStrLn $ T.unwords $ map (\c -> IMap.findWithDefault T.empty c toWord) cs

main0 = do
  initWarn
  (sets, (toInt, toWord, wc)) <- readSets
  return ()

main64 = do
  initWarn
  (sets, (toInt, toWord, wc)) <- readSets

  -- run the union find
  uf <- UF64.newUnionFind wc
  forM_ sets $ \s -> do
    case s of
      (x:xs) -> forM_ xs $ \y -> UF64.update uf x y
      []     -> return ()
  warn "--- done updating"

  roots <- UF64.rootsArray uf
  warn "--- computed roots array"

  comps <- UF64.components wc roots
  warn "--- computed components"

  forM_ comps $ emitSet toWord

main32 = do
  initWarn
  (sets, (toInt, toWord, wc)) <- readSets

  -- run the union find
  uf <- UF32.newUnionFind wc
  forM_ sets $ \s -> do
    case s of
      (x:xs) -> forM_ xs $ \y -> UF32.update uf x y
      []     -> return ()
  warn "--- done updating"

  roots <- UF32.rootsArray uf
  warn "--- computed roots array"

  comps <- UF32.components wc roots
  warn "--- computed components"

  forM_ comps $ emitSet toWord

-- interleaved processing

main64a = do
  initWarn
  n <- fmap read getLine
  uf <- UF64.newUnionFind (n*6)

  let loop = replicateM n $ do
              ws <- fmap T.words (liftIO T.getLine)
              ns <- mapM assignInt ws
              case ns of
                [] -> return ()
                (x:xs) -> liftIO $ forM_ xs $ \y -> UF64.update uf x y

  (_, (toInt, toWord, wc)) <- runStateT loop  (Hash.empty, IMap.empty, 0)

  roots <- UF64.rootsArray uf
  warn "--- computed roots array"

  comps <- UF64.components wc roots
  warn "--- computed components"

  forM_ comps $ emitSet toWord

main32a = do
  initWarn
  n <- fmap read getLine
  uf <- UF32.newUnionFind (n*6)

  let loop = replicateM n $ do
              ws <- fmap T.words (liftIO T.getLine)
              ns <- mapM assignInt ws
              case ns of
                [] -> return ()
                (x:xs) -> liftIO $ forM_ xs $ \y -> UF32.update uf x y

  (_, (toInt, toWord, wc)) <- runStateT loop  (Hash.empty, IMap.empty, 0)

  roots <- UF32.rootsArray uf
  warn "--- computed roots array"

  comps <- UF32.components wc roots
  warn "--- computed components"

  forM_ comps $ emitSet toWord

