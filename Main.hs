module Main where

import System.Environment
import Yahs.Scheme

main :: IO ()
main = do args <- getArgs
          if null args
            then runRepl >> putStrLn "bye!"
            else runFile args
