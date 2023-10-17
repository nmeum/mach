module Main where

import Mach.Parser (mkFile)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import qualified Text.ParserCombinators.Parsec as P

printAST :: FilePath -> IO ()
printAST file = do
  res <- P.parseFromFile mkFile file
  case res of
    Left err -> hPutStrLn stderr (show err)
    Right ast -> putStrLn $ show ast

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then hPutStrLn stderr "Missing file argument"
    else printAST $ head args
