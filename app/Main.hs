module Main where

import qualified Lib as L
import qualified Generate as G
import System.Environment

usage = do
  putStrLn $ unlines lns
  where
    lns = [ "Usage: app generate n      -- generate n random sets"
          , "       app generate n seed -- generate n random sets with a seed"
          , "       app < input         -- merge sets"
          ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    []                    -> L.main
    [ ('g':_) , ns ]      -> G.generateInput (read ns) Nothing
    [ ('g':_) , ns, seed] -> G.generateInput (read ns) (Just (read seed))
    _                     -> usage
 
