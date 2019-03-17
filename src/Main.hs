{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

import System.Console.CmdArgs
import Text.Parsec.String
import System.Exit

data Args = Args { file :: String }
            deriving (Show, Data, Typeable)

argsP = Args { file = def &= help "file to parse" }

-- main 
main = do 
  args <- cmdArgs argsP
  let fName = file args 
  putStrLn ("file to parse: " ++ fName ++ "\n")
