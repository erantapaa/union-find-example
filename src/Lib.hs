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
import System.IO.Unsafe
import Data.Time.Clock
import Text.Printf
import Data.IORef

startTime :: IORef UTCTime
startTime = unsafePerformIO $ getCurrentTime >>= newIORef
{-# NOINLINE startTime #-}

initWarn = do
  getCurrentTime >>= writeIORef startTime
  hSetBuffering stderr NoBuffering
  warn "starting"

warn str = do
  now <- getCurrentTime
  start <- readIORef startTime
  let diff = diffUTCTime now start
      secs = printf "%5.1f s" (realToFrac diff :: Double)
  hPutStrLn stderr $ "--- " ++ secs ++ " - " ++ str

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
  warn $ "number of sets: " ++ show n
  let loop = replicateM n $ do
              ws <- fmap T.words (liftIO T.getLine)
              mapM assignInt ws
  r@(a, s@(_,_,wc)) <- runStateT loop (Hash.empty, IMap.empty, 0)
  warn $ "number of words: " ++ show wc
  return r

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
  warn "done updating"

  roots <- UF64.rootsArray uf
  warn "computed roots array"

  comps <- UF64.components wc roots
  warn "computed components"

  forM_ comps $ emitSet toWord
  warn "done"

main32 = do
  initWarn
  (sets, (toInt, toWord, wc)) <- readSets

  -- run the union find
  uf <- UF32.newUnionFind wc
  forM_ sets $ \s -> do
    case s of
      (x:xs) -> forM_ xs $ \y -> UF32.update uf x y
      []     -> return ()
  warn "done updating"

  roots <- UF32.rootsArray uf
  warn "computed roots array"

  comps <- UF32.components wc roots
  warn "computed components"

  forM_ comps $ emitSet toWord
  warn "done"

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
  warn "computed roots array"

  comps <- UF64.components wc roots
  warn "computed components"

  forM_ comps $ emitSet toWord
  warn "done"

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
  warn "computed roots array"

  comps <- UF32.components wc roots
  warn "computed components"

  forM_ comps $ emitSet toWord
  warn "done"

-- test

mainTest = do
  initWarn
  (nsets : nnodes : _) <- fmap (map read . words) getLine
  uf <- UF64.newUnionFind nnodes
  replicateM_ nsets $ do
    nums <- fmap (map read . words) getLine
    case nums of
      [] -> return ()
      xs -> forM_ (zip xs (tail xs)) $ \(x,y) -> UF64.update uf x y
  putStr $ "sizes: "
  UF64.dumpSizes uf
  putStr $ "comps: "
  UF64.dumpComps uf

