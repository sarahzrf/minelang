module Main where

import qualified Data.Map as M
import Control.Monad

import Parse
import VM1
import Commands

writeProg :: (Command, Commands.Program) -> IO ()
writeProg (mainCmd, prog) = do
  void $ flip M.traverseWithKey prog $ \(f, b) block -> do
    let fn = "functions/f" ++ show f ++ "b" ++ show b ++ ".mcfunction"
    writeFile fn (unlines block)
  let fn = "functions/main.mcfunction"
  writeFile fn . unlines $ [
    "data modify block ~ ~ ~-3 Items set value " ++
    "[{Slot: 0b, id: \"minecraft:redstone_block\", Count: 1b, tag: " ++
    "{stack: [{}], callstack: [{ret: 'tellraw @p {\"block\": \"~ ~ ~-3\", " ++
    "\"nbt\": \"Items[0].tag.stack[0]\"}'}]}}]",
    mainCmd]

main :: IO ()
main = do
  code <- getContents
  case parseExpr =<< tokenize code of
    Left err -> putStrLn err
    Right expr -> do
      writeProg . compileProg' . compile' $ expr
      putStrLn "Done!"

