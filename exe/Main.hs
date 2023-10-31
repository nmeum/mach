module Main where

import Data.Maybe (fromJust)
import Mach.Eval (eval)
import Mach.Exec (lookupTarget, maybeBuild)
import Mach.Parser (mkFile)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import qualified Text.ParserCombinators.Parsec as P

printAST :: FilePath -> IO ()
printAST file = do
  res <- P.parseFromFile mkFile file
  case res of
    Left err -> hPutStrLn stderr (show err)
    Right ast -> do
      putStrLn $ show ast
      let mkDef = eval ast

      _ <- maybeBuild mkDef (fromJust $ lookupTarget mkDef "all")
      pure ()

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then hPutStrLn stderr "Missing file argument"
    else printAST $ head args
