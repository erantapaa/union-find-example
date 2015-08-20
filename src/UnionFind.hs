{-# LANGUAGE FlexibleContexts #-}

module UnionFind where
import Data.Array.IO
import Control.Monad
import Data.Array.MArray (freeze)
import Data.Array.Unboxed

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UV
import Data.Ord (compare)
import Data.List (groupBy)

data Merge = NoMerge Int | Merge Int Int

data UnionFind = UF { ufcomp_ :: IOUArray Int Int, ufsize_ :: IOUArray Int Int }

newUnionFind :: Int -> IO UnionFind
newUnionFind n = do
  size <- newArray (1,n) 1
  arr <- newArray (1,n) 1
  forM_ [1..n] $ \i -> writeArray arr i i
  return $ UF arr size

-- return the component associated with x
find :: UnionFind -> Int -> IO Int
find uf x = go x
  where go x = do y <- readArray (ufcomp_ uf) x
                  if x == y then return x else go y

-- associate x and y; returns a Merge value
update :: UnionFind -> Int -> Int -> IO Merge
update uf x y = do
  cx <- find uf x
  cy <- find uf y
  if cx == cy
    then return $ NoMerge cx
    else do sx <- readArray (ufsize_ uf) cx
            sy <- readArray (ufsize_ uf) cy
            if sx < sy then do writeArray (ufcomp_ uf) cx cy
                               writeArray (ufsize_ uf) cy (sx+sy)
                               return $ Merge cx cy -- cx merged into cy
                       else do writeArray (ufcomp_ uf) cy cx
                               writeArray (ufsize_ uf) cx (sx+sy)
                               return $ Merge cy cx -- cy merged into cx

isRoot uf x = do
  v <- readArray (ufcomp_ uf) x
  return $ x == v

-- return the root nodes
roots uf = do
  arr <- freeze (ufcomp_ uf) :: IO (UArray Int Int)
  return $ [ x | (x,cx) <- assocs arr, x == cx ]

-- return an array of the root values
rootsArray uf = do
  (_,n) <- getBounds (ufcomp_ uf)
  comps <- newArray (1,n) 0 :: IO (IOUArray Int Int)
  forM_ [1..n] $ \i -> do
    r <- find uf i
    writeArray comps i r
  freeze comps :: IO (UArray Int Int)

-- return lists of values in the same component
components :: Int -> UArray Int Int -> IO [[Int]]
components n comps = do
  v <- UV.new n
  forM_ [(0::Int)..(n-1)] $ \i -> UV.write v i (i+1)
  let cmp i j = compare (comps!i) (comps!j)
  VA.sortBy cmp v
  w <- UV.freeze v
  -- return lists of the the same component
  let sameComponent i j = (comps!i) == (comps!j)
  return $ groupBy sameComponent (UV.toList w)

