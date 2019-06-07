{-# LANGUAGE FlexibleContexts #-}
module VM1 where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.List

import Lang hiding (Arith)
import qualified Lang

type ProcId = Int
type Index = Int
data Literal
  = IntLit Int
  | EmptyLit
  deriving (Show, Eq, Ord)
type Path = [Symbol]
data Location
  = OnStack Index Path
  | InScope Path
  deriving (Show, Eq, Ord)
data Source = Lit Literal | Ref Location deriving (Show, Eq, Ord)
data Instruction
  = Store Location Source
  | Push Index Source
  | Pop Index
  | Arith Op
  | MkFun ProcId
  | Call
  deriving (Show, Eq, Ord)
type Proc = [Instruction]
type Program = Map ProcId Proc

type CompilerM = State (ProcId, Program)

save :: Proc -> CompilerM ProcId
save proc = state $
  \(nextId, prog) -> (nextId, (nextId + 1, M.insert nextId proc prog))

-- Produces a series of instructions whose execution causes the result of
-- evaluating the term to be stored at the top of the stack.
-- TODO error checking?
compile :: Expr -> CompilerM Proc
compile (IntExpr n) = return [Push 0 (Lit (IntLit n))]
compile (Var sym) = return [Push 0 (Ref (InScope [sym]))]
compile (CompoundExpr cexpr) = do
  ps <- traverse (compile . snd) cexpr
  let populate = reverse cexpr >>=
        \(key, _) -> [Store (OnStack 0 [key]) (Ref (OnStack 1 [])), Pop 1]
  return (concat ps ++ [Push 0 (Lit EmptyLit)] ++ populate)
compile (Proj key expr) = do
  p <- compile expr
  return (p ++ [Push 0 (Ref (OnStack 0 [key])), Pop 1])
compile (Lang.Arith op expr1 expr2) = do
  p1 <- compile expr1
  p2 <- compile expr2
  return (p1 ++ p2 ++ [Arith op])
compile (Progn exprs) = do
  ps <- traverse compile exprs
  if null ps then return [Push 0 (Lit (IntLit 0))]
             else return (intercalate [Pop 0] ps)
compile (Assign sym expr) = do
  p <- compile expr
  return (p ++ [Store (InScope [sym]) (Ref (OnStack 0 []))])
compile (Lam params body) = do
  let loadArgs = reverse params >>= \param ->
        [Store (InScope [param]) (Ref (OnStack 0 [])), Pop 0]
  p <- compile body
  procId <- save (loadArgs ++ p)
  return [MkFun procId]
compile (App fnExpr argExprs) = do
  fn <- compile fnExpr
  args <- traverse compile argExprs
  let skip = length args
  return (fn ++ concat args ++
          [Push 0 (Ref (OnStack skip [])), Pop (skip + 1), Call])

compile' :: Expr -> (ProcId, Program)
compile' t = tweak $ runState (compile t >>= save) (0, M.empty)
  where tweak (mainId, (_, prog)) = (mainId, prog)

