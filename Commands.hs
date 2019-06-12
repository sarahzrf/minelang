module Commands where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M

import Lang (Op(..))
import VM1 (ProcId, Literal(..), Location(..), Source(..),
            Instruction(..), Proc)
import qualified VM1

type BlockId = (ProcId, Int)
type Command = String
type Block = [Command]
type Program = Map BlockId Block

type CompilerM = State (BlockId, Program)

source :: Source -> String
source src = case src of
  -- NBT lists must be homogeneous, so we need to wrap ints into compounds
  Lit (IntLit n) -> "value {ival: " ++ show n ++ "}"
  Lit EmptyLit -> "value {}"
  Ref (OnStack ix) ->
    "from block ~ ~ ~-3 Items[0].tag.stack[" ++ show ix ++ "]"
  Ref (InEnv sym) ->
    "from block ~ ~ ~-3 Items[0].tag.callstack[0].env." ++ show sym

append :: Command -> CompilerM ()
append cmd = modify go
  where go (blockId, prog) = (blockId, M.adjust (++ [cmd]) blockId prog)

jump :: BlockId -> Command
jump (f, b) = concat ["function minelang:f", show f, "b", show b]

compileInstr :: Instruction -> CompilerM ()
compileInstr instr = case instr of
  StoreEnv sym src -> append . concat $ [
    "data modify block ~ ~ ~-3 Items[0].tag.callstack[0].env.", show sym,
    " set ", source src]
  Push ix src -> append . concat $ [
    "data modify block ~ ~ ~-3 Items[0].tag.stack insert ", show ix,
    " ", source src]
  Pop ix -> append . concat $ [
    "data remove block ~ ~ ~-3 Items[0].tag.stack[", show ix, "]"]
  Insert key -> do
    append . concat $ [
      "data modify block ~ ~ ~-3 Items[0].tag.stack[0].", show key,
      " set ", source (Ref (OnStack 1))]
    append "data remove block ~ ~ ~-3 Items[0].tag.stack[1]"
  ProjInstr key -> do
    append . concat $ [
      "data modify block ~ ~ ~-3 Items[0].tag.stack prepend",
      " from block ~ ~ ~-3 Items[0].tag.stack[0].", show key]
    append "data remove block ~ ~ ~-3 Items[0].tag.stack[1]"
  ArithInstr op ->
    let opName = case op of Add -> "+="; Sub -> "-="; Mul -> "*="
    in mapM_ append [
      "execute store result score b math run data get block ~ ~ ~-3 " ++
      "Items[0].tag.stack[0].ival",
      "data remove block ~ ~ ~-3 Items[0].tag.stack[0]",
      "execute store result score a math run data get block ~ ~ ~-3 " ++
      "Items[0].tag.stack[0].ival",
      "scoreboard players operation a math " ++ opName ++ " b math",
      "execute store result block ~ ~ ~-3 Items[0].tag.stack[0].ival int 1 " ++
      "run scoreboard players get a math"]
  MkFun procId -> do
    let cmd = jump (procId, 0)
    append . concat $ ["data modify block ~ ~ ~-3 Items[0].tag.stack ",
                       "prepend value {cmd: ", show cmd, "}"]
    append $ "data modify block ~ ~ ~-3 Items[0].tag.stack[0].closure " ++
             "set from block ~ ~ ~-3 Items[0].tag.callstack[0].env"
  Call -> do
    (procId, blockNo) <- gets fst
    let blockId' = (procId, blockNo + 1)
        cmd = jump blockId'
    mapM_ append [
      "data modify block ~ ~ ~-3 Items[0].tag.callstack " ++
      "prepend value {ret: " ++ show cmd ++ "}",
      "data modify block ~ ~ ~-3 Items[0].tag.callstack[0].env " ++
      "set from block ~ ~ ~-3 Items[0].tag.stack[0].closure",
      "data modify block ~ ~ ~ Command set " ++
      "from block ~ ~ ~-3 Items[0].tag.stack[0].cmd",
      "data remove block ~ ~ ~-3 Items[0].tag.stack[0]",
      "setblock ~-2 ~ ~ minecraft:redstone_block"]
    modify (\(_, prog) -> (blockId', M.insert blockId' [] prog))
  IfKeyInstr key -> mapM_ append [
    "execute if data block ~ ~ ~-3 Items[0].tag.stack[0]." ++ show key ++
    " run data remove block ~ ~ ~-3 Items[0].tag.stack[1]",
    "execute unless data block ~ ~ ~-3 Items[0].tag.stack[0]." ++ show key ++
    " run data remove block ~ ~ ~-3 Items[0].tag.stack[2]",
    "data remove block ~ ~ ~-3 Items[0].tag.stack[0]"]
  IfInstr -> mapM_ append [
    "execute store result score a math run data get block ~ ~ ~-3 " ++
    "Items[0].tag.stack[0].ival",
    "data remove block ~ ~ ~-3 Items[0].tag.stack[0]",
    "execute unless score a math matches 0" ++
    " run data remove block ~ ~ ~-3 Items[0].tag.stack[0]",
    "execute if score a math matches 0" ++
    " run data remove block ~ ~ ~-3 Items[0].tag.stack[1]"]
  CommandInstr cmd -> mapM_ append [
    "data modify block ~ ~ ~-3 Items[0].tag.stack prepend value {}",
    "execute store result block ~ ~ ~-3 Items[0].tag.stack[0].ival " ++
    "int 1 run " ++ cmd]

compileProc :: ProcId -> Proc -> CompilerM ()
compileProc procId p = do
  let blockId = (procId, 0)
  modify (\(_, prog) -> (blockId, M.insert blockId [] prog))
  mapM_ compileInstr p
  mapM_ append [
    "data modify block ~ ~ ~ Command set " ++
    "from block ~ ~ ~-3 Items[0].tag.callstack[0].ret",
    "data remove block ~ ~ ~-3 Items[0].tag.callstack[0]",
    "setblock ~-2 ~ ~ minecraft:redstone_block"]

compileProg :: VM1.Program -> CompilerM ()
compileProg = void . M.traverseWithKey compileProc

compileProg' :: (ProcId, VM1.Program) -> (Command, Program)
compileProg' (procId, prog) = (jump (procId, 0), prog')
  where (_, prog') = execState (compileProg prog) ((0, 0), M.empty)

