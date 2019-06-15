module Main where

import System.Environment
import System.Directory
import System.FilePath
import qualified Data.Map as M
import Control.Monad

import Parse
import VM1
import Commands

writeBlock :: ProgramName -> String -> Block -> IO ()
writeBlock progName blockName block = do
  let progDir  = maybe "functions" ("functions" </>) progName
      filename = progDir </> blockName <.> "mcfunction"
  createDirectoryIfMissing True progDir
  writeFile filename (unlines block)

writeProg :: ProgramName -> (Command, Commands.Program) -> IO ()
writeProg progName (mainCmd, prog) = do
  void $ flip M.traverseWithKey prog $ \(f, b) block -> do
    let blockName = "f" ++ show f ++ "b" ++ show b
    writeBlock progName blockName block
  writeBlock progName "start" (genStart mainCmd)

main :: IO ()
main = do
  writeBlock Nothing "finish" finish
  filenames <- getArgs
  let single = length filenames == 1
  forM_ filenames $ \filename -> do
    code <- readFile filename
    case parseExpr =<< tokenize code of
      Left err -> putStrLn $ "In file " ++ filename ++ ": " ++ err
      Right expr -> do
        let base = takeBaseName filename
            progName = if single then Nothing else Just base
        writeProg progName . compileProg' progName . compile' $ expr
        putStrLn $ "Wrote " ++ base ++ "!"

