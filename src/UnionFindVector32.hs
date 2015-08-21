{-# LANGUAGE FlexibleContexts #-}

module UnionFindVector32
where

import Control.Monad

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UVM
import Data.Ord (compare)
import Data.List (groupBy)
import Data.Int (Int32)

type Key = Int32
type IOKeyVector = UVM.IOVector Key

data Merge = NoMerge Int | Merge Int Int

data UnionFind = UF { ufcomp_ :: IOKeyVector, ufsize_ :: IOKeyVector }

vread :: UVM.Unbox a => UVM.IOVector a -> Key -> IO a
vread v k = UVM.read v (fromIntegral k)
{-# INLINE vread #-}

vwrite :: UVM.Unbox a => UVM.IOVector a -> Key -> a -> IO ()
vwrite v k a = UVM.write v (fromIntegral k) a
{-# INLINE vwrite #-}

(!) :: UV.Unbox a => UV.Vector a -> Key -> a
(!) v i = v UV.! (fromIntegral i)
{-# INLINE (!) #-}

newUnionFind :: Int -> IO UnionFind
newUnionFind n = do
  size <- UVM.new (n+1)
  arr <- UVM.new (n+1)
  forM_ [0..(fromIntegral n)] $ \i -> vwrite arr i i
  return $ UF arr size

-- return the component associated with x
find :: UnionFind -> Key -> IO Key
find uf x = go x
  where go :: Key -> IO Key
        go x = do y <- vread (ufcomp_ uf) x
                  if x == y then return x else go y

-- associate x and y; returns a Merge value
update :: UnionFind -> Int -> Int -> IO Merge
update uf x y = do
  cx <- find uf (fromIntegral x)
  cy <- find uf (fromIntegral y)
  if cx == cy
    then return $ NoMerge (fromIntegral cx)
    else do sx <- vread (ufsize_ uf) cx
            sy <- vread (ufsize_ uf) cy
            if sy < sx then do vwrite (ufcomp_ uf) cx cy
                               vwrite (ufsize_ uf) cx (sy+1)
                               return $ Merge (fromIntegral cx) (fromIntegral cy) -- cx merged into cy
                       else do vwrite (ufcomp_ uf) cy cx
                               vwrite (ufsize_ uf) cy (sx+1)
                               return $ Merge (fromIntegral cy) (fromIntegral cx) -- cy merged into cx

isRoot :: UnionFind -> Key -> IO Bool
isRoot uf x = do
  v <- vread (ufcomp_ uf) x
  return $ x == v :: IO Bool

-- return the root nodes
roots :: UnionFind -> IO [Key]
roots uf = do
  arr <- UV.freeze (ufcomp_ uf) :: IO (UV.Vector Key)
  let n = fromIntegral $ UV.length arr
  return $ [ x | x <- [1..n-1], x == arr UV.! (fromIntegral x) ]

-- return an array of the root values
rootsArray :: UnionFind -> IO (UV.Vector Key)
rootsArray uf = do
  let n = UVM.length (ufcomp_ uf) 
  comps <- UVM.new n :: IO (UVM.IOVector Key)
  forM_ [1..fromIntegral (n-1)] $ \i -> do
    r <- find uf i
    vwrite comps i r
  UV.freeze comps

-- return lists of values in the same component
components :: Int -> UV.Vector Key -> IO [[Int]]
components n comps = do
  let _ = n :: Int
      _ = comps :: UV.Vector Key
  v <- UVM.new n :: IO (UVM.IOVector Key)
  forM_ [0..fromIntegral (n-1)] $ \i -> vwrite v i (i+1)
  let cmp i j = compare (comps ! i) (comps ! j)
  VA.sortBy cmp v
  w <- UV.freeze v
  -- return lists of the the same component
  let sameComponent i j = cmp i j == EQ
  return $ map (map fromIntegral) $ groupBy sameComponent (UV.toList w)

{-
-}
