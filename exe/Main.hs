module Main where

import Mach.Main (run)
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.IO (stdout)

main :: IO ()
main = getArgs >>= run stdout >>= exitWith
