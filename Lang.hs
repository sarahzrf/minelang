{-# LANGUAGE TypeFamilies #-}
module Lang where

import Data.String
import GHC.Exts
import Control.Monad.State
import Control.Monad.Except
import Data.Functor.Compose
import Data.Map (Map)
import qualified Data.Map as M

type Symbol = String

data Op = Add | Sub | Mul deriving (Show, Eq, Ord)

runOp :: Num a => Op -> a -> a -> a
runOp Add = (+)
runOp Sub = (-)
runOp Mul = (*)

data Expr
  = IntExpr Int
  | Var Symbol
  | CompoundExpr [(Symbol, Expr)]
  | ProjExpr Symbol Expr
  | ArithExpr Op Expr Expr
  | Progn [Expr]
  | Assign Symbol Expr
  | Lam (Maybe Symbol) [Symbol] Expr
  | App Expr [Expr]
  | IfKeyExpr Expr Symbol Expr Expr
  | IfExpr Expr Expr Expr
  | CommandExpr String
  deriving (Show, Eq, Ord)

instance IsString Expr where
  fromString = Var
instance IsList Expr where
  type (Item Expr) = Expr
  fromList [] = error "fake instance"
  fromList (fn:args) = App fn args
  toList = error "fake instance"
instance Num Expr where
  expr1 + expr2 = ArithExpr Add expr1 expr2
  expr1 - expr2 = ArithExpr Sub expr1 expr2
  expr1 * expr2 = ArithExpr Mul expr1 expr2
  abs = error "fake instance"
  signum = error "fake instance"
  fromInteger = IntExpr . fromInteger


data Val
  = IntVal Int
  | CompoundVal (Map Symbol Val)
  | FunVal (Maybe Symbol) [Symbol] Expr Env
  deriving (Show, Eq, Ord)
type Env = Map Symbol Val

eval :: Expr -> StateT Env (Either String) Val
eval expr' = case expr' of
  IntExpr n -> return (IntVal n)
  Var sym -> do
    mv <- gets (M.lookup sym)
    maybe (throwError "unbound variable") return mv
  CompoundExpr cexpr ->
    CompoundVal . M.fromList . getCompose <$> traverse eval (Compose cexpr)
  ProjExpr key expr -> do
    m <- eval expr
    case m of
      CompoundVal c | Just v <- M.lookup key c -> return v
                    | otherwise -> throwError "nonexistent key"
      _ -> throwError "not a compound"
  ArithExpr op expr1 expr2 -> do
    v1 <- eval expr1
    v2 <- eval expr2
    case (v1, v2) of
      (IntVal n, IntVal m) -> return (IntVal (runOp op n m))
      _ -> throwError "not ints"
  Progn exprs -> if null exprs then return (IntVal 0)
                             else last <$> traverse eval exprs
  Assign sym expr -> do
    v <- eval expr
    modify (M.insert sym v)
    return v
  Lam name params body -> FunVal name params body <$> get
  App fnExpr argsExpr -> do
    fn <- eval fnExpr
    args <- traverse eval argsExpr
    case fn of
      FunVal name params body closure
        | length params == length args ->
          let selfRef = maybe M.empty (flip M.singleton fn) name
              env = M.unions [M.fromList (zip params args), selfRef, closure]
          in lift (evalStateT (eval body) env)
        | otherwise -> throwError "wrong number of arguments"
      _ -> throwError "not a function"
  IfKeyExpr cond key th el -> do
    m <- eval cond
    case m of
      CompoundVal c | key `M.member` c -> evalSubscope th
                    | otherwise -> evalSubscope el
      _ -> throwError "not a compound"
  IfExpr cond th el -> do
    m <- eval cond
    case m of
      IntVal 0 -> evalSubscope el
      IntVal _ -> evalSubscope th
      _ -> throwError "not an int"
  CommandExpr _ -> return (IntVal 0)

evalSubscope :: Expr -> StateT Env (Either String) Val
evalSubscope expr = eval (App (Lam Nothing [] expr) [])

eval' :: Expr -> Either String Val
eval' t = evalStateT (eval t) M.empty

