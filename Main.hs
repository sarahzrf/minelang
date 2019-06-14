module Main where

import System.Environment
import System.Directory
import System.FilePath
import qualified Data.Map as M
import Control.Monad

import Parse
import VM1
import Commands

writeProg :: ProgramName -> (Command, Commands.Program) -> IO ()
writeProg progName (mainCmd, prog) = do
  let progDir = maybe "functions" ("functions" </>) progName
      funcFilename funcName = progDir </> funcName <.> "mcfunction"
  createDirectoryIfMissing True progDir
  void $ flip M.traverseWithKey prog $ \(f, b) block -> do
    let filename = funcFilename $ "f" ++ show f ++ "b" ++ show b
    writeFile filename (unlines block)
  writeFile (funcFilename "main") . unlines $ [
    "data modify " ++ storage ++ " Items set value\
    \ [{Slot: 0b, id: \"minecraft:redstone_block\", Count: 1b, tag:\
    \ {stack: [{}], callstack: [{ret: '" ++ final ++ "'}]}}]",
    mainCmd]

main :: IO ()
main = do
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

