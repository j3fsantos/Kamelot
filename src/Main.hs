{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

import System.Console.CmdArgs
import Text.Parsec.String
import System.Exit
import Syntax.Parsing.Formula
import System.IO

data Args = Args { file :: String }
            deriving (Show, Data, Typeable)

argsP = Args { file = def &= help "file to parse" }

-- main 
main = do 
  args <- cmdArgs argsP
  let fName = file args 
  putStrLn ("file to parse: " ++ fName)
  parseFromFile pFormList fName >>= either reportErr reportSucc
  where
    reportErr err = do
      hPutStrLn stderr $ "Error: " ++ show err
      exitFailure
    --
    reportSucc succ = do 
      putStrLn ("list size: " ++ (show (length succ)))
      putStrLn ("Parsed: " ++ (show succ))