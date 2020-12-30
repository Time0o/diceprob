{-# LANGUAGE DeriveDataTypeable #-}

module Diceprob where

import System.Console.CmdArgs
import System.Exit (die)

import AST (buildAST, debugAST)

data Diceprob = Diceprob {script :: String}
                deriving (Show, Data, Typeable)

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
    Nothing -> error "no input file specified"
    Just (script) -> case buildAST script of
      (Left parseError) -> putStrLn . debugAST $ parseError
      (Right stmt) -> putStrLn . show $ stmt
