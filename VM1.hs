{-# LANGUAGE FlexibleContexts #-}
module VM1 where

import Control.Monad.State
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import Data.List

import Lisp

type ProcId = Int
type Index = Int
data Literal
  = IntLit Int
  | NilLit
  | ConsLit Literal Literal
  deriving (Show, Eq, Ord)
data Step = InCar | InCdr deriving (Show, Eq, Ord)
type Path = [Step]
data Location
  = OnStack Index Path
  | InScope String
  deriving (Show, Eq, Ord)
data Source = Lit Literal | Ref Location deriving (Show, Eq, Ord)
data Op = Add | Sub | Mul deriving (Show, Eq, Ord)
data Instruction
  = Store Location Source
  | Push Index Source
  | Cons
  | Pop Index
  | Arith Op
  | MkFun ProcId
  | Call
  deriving (Show, Eq, Ord)
type Proc = [Instruction]
type Program = Map ProcId Proc

type CompilerM = StateT (ProcId, Program) (Either String)

save :: Proc -> CompilerM ProcId
save proc = state $
  \(nextId, prog) -> (nextId, (nextId + 1, M.insert nextId proc prog))

-- Produces a series of instructions whose execution causes the result of
-- evaluating the term to be stored at the top of the stack.
-- TODO error checking?
compile :: LispTerm -> CompilerM Proc
compile (IntTerm n) = return [Push 0 (Lit (IntLit n))]
compile (SymTerm sym) = do
  void $ checkSym sym
  return [Push 0 (Ref (InScope sym))]
compile app = case getListTerm app of
  Just [] -> return [Push 0 (Lit NilLit)]
  Just [SymTerm "cons", car, cdr] -> do
    p1 <- compile car
    p2 <- compile cdr
    return (p1 ++ p2 ++ [Cons])
  Just [SymTerm sym, t]
    | Just step <- lookup sym [("car", InCar), ("cdr", InCdr)] -> do
      p <- compile t
      return (p ++ [Push 0 (Ref (OnStack 0 [step])), Pop 1])
  Just [SymTerm sym, t1, t2]
    | Just op <- lookup sym [("+", Add), ("*", Mul), ("-", Sub)] -> do
      p1 <- compile t1
      p2 <- compile t2
      return (p1 ++ p2 ++ [Arith op])
  Just (SymTerm "progn":ts) -> do
    ps <- traverse compile ts
    if null ps then return [Push 0 (Lit NilLit)]
               else return (intercalate [Pop 0] ps)
  Just [SymTerm "set", SymTerm sym, t] -> do
    void $ checkSym sym
    p <- compile t
    return (p ++ [Store (InScope sym) (Ref (OnStack 0 []))])
  Just [SymTerm "lambda", paramsT, body] -> do
    let getParam (SymTerm sym) = checkSym sym
        getParam _ = throwError "bad param"
        pm = getListTerm paramsT
    params <- maybe (throwError "bad params") (traverse getParam) pm
    let loadArgs = reverse params >>= \param ->
          [Store (InScope param) (Ref (OnStack 0 [])), Pop 0]
    p <- compile body
    procId <- save (loadArgs ++ p)
    return [MkFun procId]
  Just (fnT:argsT) -> do
    fn <- compile fnT
    args <- traverse compile argsT
    let skip = length args
    return (fn ++ concat args ++
            [Push 0 (Ref (OnStack skip [])), Pop (skip + 1), Call])
  _ -> throwError "bad application"

compile' :: LispTerm -> Either String (ProcId, Program)
compile' t = tweak <$> runStateT (compile t >>= save) (0, M.empty)
  where tweak (mainId, (_, prog)) = (mainId, prog)

