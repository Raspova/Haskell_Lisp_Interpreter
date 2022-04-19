module Main where
import System.Environment
import System.Exit
import Lib
import OldLib
    
import Control.Monad (forever)

readFiles:: [FilePath] -> IO [String]
readFiles = mapM readFile

-- getL::  IO 
--         res <- getLine
--         return res
-- 

main :: IO ()
main = do 
    args <- getArgs
    a <- readFiles args
    -- multiple lines feed for test/test:
    mapM_ evalAndPrintLisp (lines (head a )) 
    -- real one : 
    -- evalAndPrintLisp (concat (map (eraseAll "\n") a))