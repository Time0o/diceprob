{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveDataTypeable #-}

import Prelude hiding (putStrLn)

import Data.Text.IO (putStrLn)

import System.Console.CmdArgs

import Diceprob.AST (buildAST, debugAST)
import Diceprob.Eval (eval, evalStmt)
import Diceprob.Output
import Diceprob.Text (textShow)

data Diceprob = Diceprob {script :: String}
                deriving (Data, Show, Typeable)

diceprob :: Diceprob
diceprob = Diceprob{script = def &= args &= typ "SCRIPT"}
         &= help "Run Anydice script locally"
         &= summary "Diceprob v0.0.0, (C) Timo Nicolai"

readScript :: IO (Maybe (String, String))
readScript = do
  scriptName <- script <$> cmdArgs diceprob
  if null scriptName then
    return Nothing
  else do
    scriptContent <- readFile scriptName
    return $ Just (scriptName, scriptContent)

main :: IO ()
main = do
  maybeScript <- readScript
  case maybeScript of
    Nothing  -> error "no input file specified"
    Just buf -> case buildAST buf of
      Left parseError -> putStrLn . textShow . debugAST $ parseError
      Right stmt      -> putStrLn . outputsToExport . eval evalStmt $ stmt
