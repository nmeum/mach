module Main where

import Mach.Main (run)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= run
