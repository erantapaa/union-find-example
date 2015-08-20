{-# LANGUAGE NoMonomorphismRestriction #-}

module Generate
where

import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans
import System.Random

-- generate a random word like B0055URMG0
-- L DDDD LLLL D

randomWord = do
  n <- getRandomR (0, 9999)
  let _ = n :: Int
  alphas <- replicateM 4 $ getRandomR ('A', 'E')
  d <- getRandomR ('0','0')
  let digits = show n
      padded = take (4-(length digits)) "0000" ++ digits
  return $ concat [ "B", padded, alphas, [d] ]

randomSet = do
  k <- getRandomR (2,10)
  replicateM k randomWord

doit n = do
  replicateM_ n $ do
    s <- randomSet
    liftIO $ putStrLn $ unwords s

generateInput n seed = do
  g <- case seed of
         Nothing  -> newStdGen
         Just x   -> return $ mkStdGen x
  putStrLn $ show n
  evalRandT (doit n) g

