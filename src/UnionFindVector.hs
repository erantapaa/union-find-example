{-# LANGUAGE FlexibleContexts #-}

module UnionFindVector
where

import Control.Monad

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UVM
import Data.Ord (compare)
import Data.List (groupBy)

data Merge = NoMerge Int | Merge Int Int

data UnionFind = UF { ufcomp_ :: UVM.IOVector Int, ufsize_ :: UVM.IOVector Int }

newUnionFind :: Int -> IO UnionFind
newUnionFind n = do
  size <- UVM.replicate (n+1) 1
  arr <- UVM.new (n+1)
  forM_ [0..n] $ \i -> UVM.write arr i i
  return $ UF arr size

-- return the component associated with x updating 
-- nodes to point to their grandparents 
find' :: UnionFind -> Int -> IO Int
find' uf x = do y <- UVM.read (ufcomp_ uf) x
                if x == y
                  then return x
                  else do z <- UVM.read (ufcomp_ uf) y
                          UVM.write (ufcomp_ uf) y z
                          find' uf z

find :: UnionFind -> Int -> IO Int
find uf x = do y <- UVM.read (ufcomp_ uf) x
               if x == y
                 then return x
                 else find uf y

-- associate x and y; returns a Merge value
update :: UnionFind -> Int -> Int -> IO Merge
update uf x y = do
  cx <- find uf x
  cy <- find uf y
  if cx == cy
    then return $ NoMerge cx
    else combine uf cx cy

update' uf x y = do
  cx <- find' uf x
  cy <- find' uf y
  if cx == cy
    then return $ NoMerge cx
    else combine uf cx cy

combine :: UnionFind -> Int -> Int -> IO Merge
combine uf cx cy  =
    do sx <- UVM.read (ufsize_ uf) cx
       sy <- UVM.read (ufsize_ uf) cy
       if sx < sy then do UVM.write (ufcomp_ uf) cx cy        -- make cx point to cy
                          UVM.write (ufsize_ uf) cy (sx+sy)
                          return $ Merge cx cy -- cx merged into cy
                  else do UVM.write (ufcomp_ uf) cy cx        -- make cy point to cx
                          UVM.write (ufsize_ uf) cx (sx+sy)
                          return $ Merge cy cx -- cy merged into cx

isRoot uf x = do
  v <- UVM.read (ufcomp_ uf) x
  return $ x == v :: IO Bool

-- return the root nodes
roots :: UnionFind -> IO [Int]
roots uf = do
  arr <- UV.freeze (ufcomp_ uf) :: IO (UV.Vector Int)
  let n = UV.length arr
  return $ [ x | x <- [1..n-1], x == arr UV.! x ]

-- return an array of the root values
rootsArray :: UnionFind -> IO (UV.Vector Int)
rootsArray uf = do
  let n = UVM.length (ufcomp_ uf)
  comps <- UVM.new n :: IO (UVM.IOVector Int)
  forM_ [1..(n-1)] $ \i -> do
    r <- find uf i
    UVM.write comps i r
  UV.freeze comps

-- return an array of the root values - use find'
rootsArray' :: UnionFind -> IO (UV.Vector Int)
rootsArray' uf = do
  let n = UVM.length (ufcomp_ uf)
  comps <- UVM.new n :: IO (UVM.IOVector Int)
  forM_ [1..(n-1)] $ \i -> do
    r <- find' uf i
    UVM.write comps i r
  UV.freeze comps

-- return lists of values in the same component
components :: Int -> UV.Vector Int -> IO [[Int]]
components n comps = do
  v <- UVM.new n :: IO (UVM.IOVector Int)
  forM_ [(0::Int)..n-1] $ \i -> UVM.write v i (i+1)
  let cmp i j = compare (comps UV.! i) (comps UV.! j)
  VA.sortBy cmp v
  w <- UV.freeze v
  -- return lists of the the same component
  let sameComponent i j = (comps UV.! i) == (comps UV.! j)
  return $ groupBy sameComponent (UV.toList w)

-- return groups of indices which are in the same component
components' n roots = do
  v <- UVM.new n :: IO (UVM.IOVector Int)
  forM_ [(0::Int)..n-1] $ \i -> UVM.write v i (i+1)
  let cmp i j = compare (roots UV.! i) (roots UV.! j)
      sameComponent i j = cmp i j == EQ
  VA.sortBy cmp v
  w <- UV.freeze v
  return $ groupBy sameComponent (UV.toList w)

components'' n roots = do
  v <- UVM.new n :: IO (UVM.IOVector Int)
  forM_ [(0::Int)..n-1] $ \i -> UVM.write v i (i+1)
  let cmp i j = compare (roots V.! i) (roots V.! j)
      sameComponent i j = cmp i j == EQ
  VA.sortBy cmp v
  w <- UV.freeze v
  return $ groupBy sameComponent (UV.toList w)

dumpSizes uf = do
  v <- UV.freeze (ufsize_ uf)
  putStrLn $ unwords $ map show (UV.toList v)

dumpComps uf = do
  v <- UV.freeze (ufcomp_ uf)
  putStrLn $ unwords $ map show (UV.toList v)

