module VM1 where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import Data.List
import Data.Bifunctor

import Lang (Symbol, Op(..), runOp, Expr(..))

type ProcId = Int
type Index = Int
data Literal
  = IntLit Int
  | EmptyLit
  deriving (Show, Eq, Ord)
data Location
  = OnStack Index
  | InEnv Symbol
  deriving (Show, Eq, Ord)
data Source = Lit Literal | Ref Location deriving (Show, Eq, Ord)
data Instruction
  = StoreEnv Symbol Source
  | Push Index Source
  | Pop Index
  | Insert Symbol
  | ProjInstr Symbol
  | ArithInstr Op
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
compile (Var sym) = return [Push 0 (Ref (InEnv sym))]
compile (CompoundExpr cexpr) = do
  ps <- traverse (compile . snd) cexpr
  let populate = map (Insert . fst) (reverse cexpr)
  return (concat ps ++ [Push 0 (Lit EmptyLit)] ++ populate)
compile (ProjExpr key expr) = do
  p <- compile expr
  return (p ++ [ProjInstr key])
compile (ArithExpr op expr1 expr2) = do
  p1 <- compile expr1
  p2 <- compile expr2
  return (p1 ++ p2 ++ [ArithInstr op])
compile (Progn exprs) = do
  ps <- traverse compile exprs
  if null ps then return [Push 0 (Lit (IntLit 0))]
             else return (intercalate [Pop 0] ps)
compile (Assign sym expr) = do
  p <- compile expr
  return (p ++ [StoreEnv sym (Ref (OnStack 0))])
compile (Lam params body) = do
  let loadArgs = reverse params >>= \param ->
        [StoreEnv param (Ref (OnStack 0)), Pop 0]
  p <- compile body
  procId <- save (loadArgs ++ p)
  return [MkFun procId]
compile (App fnExpr argExprs) = do
  fn <- compile fnExpr
  args <- traverse compile argExprs
  let skip = length args
  return (fn ++ concat args ++
          [Push 0 (Ref (OnStack skip)), Pop (skip + 1), Call])

compile' :: Expr -> (ProcId, Program)
compile' t = tweak $ runState (compile t >>= save) (0, M.empty)
  where tweak (mainId, (_, prog)) = (mainId, prog)


data Val
  = IntVal Int
  | CompoundVal (Map Symbol Val)
  | FunVal ProcId Env
  deriving (Show, Eq, Ord)
type Env = Map Symbol Val

type VM1State = ([Val], Env)
type VM1M = ReaderT Program (StateT VM1State (Either String))

unwrap :: String -> VM1M (Maybe a) -> VM1M a
unwrap err m = m >>= maybe (throwError err) return

evalLit :: Literal -> Val
evalLit (IntLit n) = IntVal n
evalLit EmptyLit = CompoundVal M.empty

evalLoc :: Location -> VM1M Val
evalLoc (OnStack ix) = do
  stack <- gets fst
  case drop ix stack of
    [] -> throwError "nonexistent stack index"
    v:_ -> return v
evalLoc (InEnv sym) = unwrap "nonexistent binding" (gets (M.lookup sym . snd))

evalSource :: Source -> VM1M Val
evalSource (Lit lit) = return (evalLit lit)
evalSource (Ref loc) = evalLoc loc

pop0 :: VM1M Val
pop0 = unwrap "stack empty" (state go)
  where go ([], env) = (Nothing, ([], env))
        go (v:vs, env) = (Just v, (vs, env))

push0 :: Val -> VM1M ()
push0 v = modify (first (v:))

runInstr :: Instruction -> VM1M ()
runInstr instr = case instr of
  StoreEnv sym src -> do
    v <- evalSource src
    modify (second (M.insert sym v))
  Push ix src -> do
    v <- evalSource src
    let ins l = let (pre, post) = splitAt ix l in pre ++ [v] ++ post
    modify (first ins)
  Pop ix -> let del l = let (pre, post) = splitAt ix l in pre ++ drop 1 post
            in modify (first del)
  Insert key -> do
    v0 <- pop0
    compound <- case v0 of
      CompoundVal compound -> return compound
      _ -> throwError "not a compound"
    v <- pop0
    let newV0 = CompoundVal (M.insert key v compound)
    push0 newV0
  ProjInstr key -> do
    v0 <- pop0
    compound <- case v0 of
      CompoundVal compound -> return compound
      _ -> throwError "not a compound"
    v <- unwrap "nonexistent key" (return (M.lookup key compound))
    push0 v
  ArithInstr op -> do
    v1 <- pop0
    v2 <- pop0
    case (v1, v2) of
      (IntVal n, IntVal m) -> push0 (IntVal (runOp op n m))
      _ -> throwError "not ints"
  MkFun procId -> FunVal procId <$> gets snd >>= push0
  Call -> do
    v0 <- pop0
    (procId, closure) <- case v0 of
      FunVal procId closure -> return (procId, closure)
      _ -> throwError "not a function"
    saved <- gets snd
    modify (second (const closure))
    runProc procId
    modify (second (const saved))

runProc :: ProcId -> VM1M ()
runProc procId = do
  p <- unwrap "no such proc" (asks (M.lookup procId))
  mapM_ runInstr p

runProc' :: (ProcId, Program) -> Either String Val
runProc' (procId, prog) = case e of
    Right (v:_, _) -> Right v
    Right ([], _) -> Left "no result?!"
    Left err -> Left err
  where e = execStateT (runReaderT (runProc procId) prog) ([], M.empty)

