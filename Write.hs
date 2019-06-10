module Write where

import qualified Data.Map as M
import Control.Monad

import Commands

writeProg :: (Command, Program) -> IO ()
writeProg (main, prog) = do
  void $ flip M.traverseWithKey prog $ \(f, b) block -> do
    let fn = "functions/f" ++ show f ++ "b" ++ show b ++ ".mcfunction"
    writeFile fn (unlines block)
  let fn = "functions/main.mcfunction"
  writeFile fn . unlines $ [
    "data modify block ~ ~ ~-3 Items set value " ++
    "[{Slot: 0b, id: \"minecraft:redstone_block\", Count: 1b, tag: " ++
    "{stack: [{}], callstack: [{ret: 'tellraw @p {\"block\": \"~ ~ ~-3\", " ++
    "\"nbt\": \"Items[0].tag.stack[0]\"}'}]}}]",
    main]

