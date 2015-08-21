module Main where

import qualified Lib as L
import qualified Generate as G
import System.Environment

usage = do
  putStrLn $ unlines lns
  where
    lns = [ "Usage: app generate n        -- generate n random sets"
          , "       app generate n seed   -- generate n random sets with a seed"
          , "       app merge64 < input   -- merge sets using union find for Ints"
          , "       app merge32 < input   -- merge sets using union find for Int32"
          , "       app merge64a < input  -- interleaved merge sets using union find for Ints"
          , "       app merge32a < input  -- interleaved merge sets using union find for Int32"
          , "       app test < input      -- merge integer only sets"
          ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ ('g':_) , ns ]      -> G.generateInput (read ns) Nothing
    [ ('g':_) , ns, seed] -> G.generateInput (read ns) (Just (read seed))
    ( "merge64" : _)      -> L.main64
    ( "merge32" : _)      -> L.main32
    ( "merge0"  : _)      -> L.main0
    ( "merge64a" : _)     -> L.main64a
    ( "merge32a" : _)     -> L.main32a
    ( "test" : _)         -> L.mainTest 
    _                     -> usage
 
