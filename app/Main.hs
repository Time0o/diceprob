{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DeriveDataTypeable #-}

import System.Console.CmdArgs

import Diceprob.AST (buildAST, debugAST)
import Diceprob.Eval (eval, evalStmt)

data Diceprob = Diceprob {script :: String}
                deriving (Show, Data, Typeable)

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
    Nothing -> error "no input file specified"
    Just (buf) -> case buildAST buf of
      (Left parseError) -> putStrLn . debugAST $ parseError
      (Right stmt) -> putStrLn . show . eval evalStmt $ stmt
