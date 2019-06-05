{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
module Lisp where

import Data.String
import GHC.Exts
import Control.Monad.State
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M

type Symbol = String

{-
- Special forms:
- * (cons exp exp)
- * (car exp)
- * (cdr exp)
- * (+ exp exp)
- * (- exp exp)
- * (* exp exp)
- * (progn exp*)
- * (set var exp)
- * (lambda (var*) exp)
-}

data LispTerm
  = IntTerm Int
  | SymTerm Symbol
  | NilTerm
  | ConsTerm LispTerm LispTerm
  deriving (Show, Eq, Ord)

getListTerm :: LispTerm -> Maybe [LispTerm]
getListTerm NilTerm = Just []
getListTerm (ConsTerm car cdr) = (car:) <$> getListTerm cdr
getListTerm _ = Nothing

instance IsString LispTerm where
  fromString = SymTerm
instance IsList LispTerm where
  type (Item LispTerm) = LispTerm
  fromList = foldr ConsTerm NilTerm
  toList = fail "fake instance"
instance Num LispTerm where
  t1 + t2 = ["+", t1, t2]
  t1 - t2 = ["-", t1, t2]
  t1 * t2 = ["*", t1, t2]
  abs = error "fake instance"
  signum = error "fake instance"
  fromInteger = IntTerm . fromInteger


data LispVal
  = IntVal Int
  | NilVal
  | ConsVal LispVal LispVal
  | FunVal [Symbol] LispTerm Env
  deriving (Show, Eq, Ord)
type Env = Map Symbol LispVal

specialForms :: [String]
specialForms = words "cons car cdr + - * progn set lambda"

checkSym :: Symbol -> StateT Env (Either String) ()
checkSym sym
  | sym `elem` specialForms = throwError "reserved name"
  | otherwise = return ()

eval :: LispTerm -> StateT Env (Either String) LispVal
eval (IntTerm n) = return (IntVal n)
eval (SymTerm sym) = do
  checkSym sym
  mv <- gets (M.lookup sym)
  maybe (throwError "unbound variable") return mv
eval app = case getListTerm app of
  Just [] -> return NilVal
  Just [SymTerm "cons", car, cdr] -> ConsVal <$> eval car <*> eval cdr
  Just [SymTerm sym, t]
    | Just op <- lookup sym [("car", fst), ("cdr", snd)] -> do
    v <- eval t
    case v of
      ConsVal car cdr -> return (op (car, cdr))
      _ -> throwError "not a cons cell"
  Just [SymTerm sym, t1, t2]
    | Just op <- lookup sym [("+", (+)), ("*", (*)), ("-", (-))] -> do
    v1 <- eval t1
    v2 <- eval t2
    case (v1, v2) of
      (IntVal n, IntVal m) -> return (IntVal (op n m))
      _ -> throwError "not ints"
  Just (SymTerm "progn":ts) -> do
    vs <- traverse eval ts
    if null vs then return NilVal else return (last vs)
  Just [SymTerm "set", SymTerm sym, t] -> do
    checkSym sym
    v <- eval t
    modify (M.insert sym v)
    return v
  Just [SymTerm "lambda", paramsT, body] -> do
    let getParam (SymTerm sym) = return sym
        getParam _ = throwError "bad param"
        pm = getListTerm paramsT
    params <- maybe (throwError "bad params") (traverse getParam) pm
    closure <- get
    return (FunVal params body closure)
  Just (fnT:argsT) -> do
    fn <- eval fnT
    args <- traverse eval argsT
    case fn of
      FunVal params body closure
        | length params == length args ->
          let env = closure `M.union` M.fromList (zip params args)
          in lift (evalStateT (eval body) env)
        | otherwise -> throwError "wrong number of arguments"
      _ -> throwError "not a function"
  _ -> throwError "bad application"

eval' :: LispTerm -> Either String LispVal
eval' t = evalStateT (eval t) M.empty

