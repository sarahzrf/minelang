{-# LANGUAGE QuasiQuotes #-}
module Commands where

import Prelude hiding (mod, rem)
import Control.Monad.State
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as M
import Data.Bifunctor
import PyF

import Lang (Op(..))
import VM1 (ProcId, Literal(..), Location(..), Source(..),
            Instruction(..), Proc)
import qualified VM1

type BlockId = (ProcId, Int)
type Command = String
type Block = [Command]
type Program = Map BlockId Block
type ProgramName = Maybe String

type CompilerM = ReaderT ProgramName (State (BlockId, Program))

storageSpot, pulseSpot, storage, stack, callstack, env, math :: String
storageSpot = "~ ~1 ~"
pulseSpot = "~ ~-2 ~"
storage = "block " ++ storageSpot
stack = storage ++ " Items[0].tag.stack"
callstack = storage ++ " Items[0].tag.callstack"
env = callstack ++ "[0].env"
math = "minelang_math"

setup :: Block
setup = [
  [f|fill ~ ~-2 ~ ~ ~1 ~ minecraft:air|],
  [f|setblock ~ ~-1 ~ minecraft:command_block[facing=up]|] ++
    [f|{{Command: "setblock ~ ~-1 ~ minecraft:air" }}|],
  [f|setblock ~ ~ ~ minecraft:chain_command_block{{auto: 1b}}|],
  [f|setblock ~ ~1 ~ minecraft:furnace{furnaceDat}|]]
  where furnaceDat = "{Items: [{Slot: 0b, id: 'minecraft:redstone_block',\
          \ Count: 1b, tag: {stack: [], callstack:\
                             \ [{ret: 'function minelang:finish'}]}}]}"

finish :: Block
finish = [
  [f|tellraw @p {finalMsg}|],
  [f|fill ~ ~-2 ~ ~ ~1 ~ minecraft:air|]]
  where finalMsg :: String
        finalMsg =
          [f|{{"block": "{storageSpot}", "nbt": "Items[0].tag.stack[0]"}}|]

mod, rem, estore :: String
mod = "data modify"
rem = "data remove"
estore = "execute store result"

source :: Source -> String
source src = case src of
  -- NBT lists must be homogeneous, so we need to wrap ints into compounds
  Lit (IntLit n) -> [f|value {{ival: {n}}}|]
  Lit EmptyLit -> [f|value {{}}|]
  Ref (OnStack ix) -> [f|from {stack}[{ix}]|]
  Ref (InEnv sym) -> [f|from {env}.{show sym}|]

append :: [Command] -> CompilerM ()
append1 :: Command -> CompilerM ()
append cmds = modify go
  where go (blockId, prog) = (blockId, M.adjust (++ cmds) blockId prog)
append1 = append . pure

jump :: BlockId -> ProgramName -> Command
jump (f', b) progName = [f|function minelang:{dir}f{f'}b{b}|]
  where dir = maybe "" (++ "/") progName

jump' :: BlockId -> CompilerM Command
jump' = asks . jump

compileInstr :: Instruction -> CompilerM ()
compileInstr instr = case instr of
  StoreEnv sym src -> append1 [f|{mod} {env}.{show sym} set {source src}|]
  Push ix src -> append1 [f|{mod} {stack} insert {ix} {source src}|]
  Pop ix -> append1 [f|{rem} {stack}[{ix}]|]
  Insert key -> append [
    [f|{mod} {stack}[0].{show key} set from {stack}[1]|],
    [f|{rem} {stack}[1]|]]
  ProjInstr key -> append [
    [f|{mod} {stack} prepend from {stack}[0].{show key}|],
    [f|{rem} {stack}[1]|]]
  ArithInstr op ->
    let opName = case op of Add -> "+="; Sub -> "-="; Mul -> "*="
    in append [
      [f|{estore} score a {math} run data get {stack}[1].ival|],
      [f|{estore} score b {math} run data get {stack}[0].ival|],
      [f|{rem} {stack}[0]|],
      [f|scoreboard players operation a {math} {opName} b {math}|],
      [f|{estore} {stack}[0].ival int 1 run scoreboard players get a {math}|]]
  MkFun procId -> do
    cmd <- jump' (procId, 0)
    append [
      [f|{mod} {stack} prepend value {{cmd: {show cmd}}}|],
      [f|{mod} {stack}[0].closure set from {env}|]]
  Call -> do
    (procId, blockNo) <- gets fst
    let blockId' = (procId, blockNo + 1)
    cmd <- jump' blockId'
    append [
      [f|{mod} {callstack} prepend value {{ret: {show cmd}}}|],
      [f|{mod} {env} set from {stack}[0].closure|],
      [f|{mod} block ~ ~ ~ Command set from {stack}[0].cmd|],
      [f|{rem} {stack}[0]|],
      [f|setblock {pulseSpot} minecraft:redstone_block|]]
    modify (\(_, prog) -> (blockId', M.insert blockId' [] prog))
  IfKeyInstr key -> append [
    [f|execute if data {stack}[0].{show key} run {rem} {stack}[1]|],
    [f|execute unless data {stack}[0].{show key} run {rem} {stack}[2]|],
    [f|{rem} {stack}[0]|]]
  IfInstr -> append [
    [f|{estore} score a {math} run data get {stack}[0].ival|],
    [f|{rem} {stack}[0]|],
    [f|execute unless score a {math} matches 0 run {rem} {stack}[0]|],
    [f|execute if score a {math} matches 0 run {rem} {stack}[1]|]]
  CommandInstr cmd -> append [
    [f|{mod} {stack} prepend value {{}}|],
    [f|{estore} {stack}[0].ival int 1 run {cmd'}|]]
    where cmd' = cmd >>= expand
          expand '$' = env ++ "."
          expand c = [c]

compileProc :: ProcId -> Proc -> CompilerM ()
compileProc _ [] = return ()
compileProc procId p = do
  let blockId = (procId, 0)
  modify (\(_, prog) -> (blockId, M.insert blockId [] prog))
  mapM_ compileInstr (init p)
  case last p of
    Call -> append [
      [f|{mod} {env} set from {stack}[0].closure|],
      [f|{mod} block ~ ~ ~ Command set from {stack}[0].cmd|],
      [f|{rem} {stack}[0]|],
      [f|setblock {pulseSpot} minecraft:redstone_block|]]
    i -> compileInstr i >> append [
      [f|{mod} block ~ ~ ~ Command set from {callstack}[0].ret|],
      [f|{rem} {callstack}[0]|],
      [f|setblock {pulseSpot} minecraft:redstone_block|]]

compileProg :: VM1.Program -> CompilerM ()
compileProg = void . M.traverseWithKey compileProc

compileProg' :: ProgramName -> (ProcId, VM1.Program) -> (Command, Program)
compileProg' name (procId, prog) = second snd $
  runState (runReaderT act name) ((0, 0), M.empty)
  where act = compileProg prog >> jump' (procId, 0)

