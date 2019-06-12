{-# LANGUAGE RecursiveDo #-}
module Parse where

import Data.Char
import Data.Tuple
import Data.List
import Data.Maybe
import Control.Applicative
import Data.Foldable
import Text.Earley
import Text.Earley.Mixfix

import Lang

data Token
  = LParenT | RParenT
  | NatT Int
  | VarT String
  | LBraceT | RBraceT | ColonT | CommaT
  | DotT
  | AddT | SubT | MulT
  | SemiT
  | EqT
  | ArrT
  | IfkT | IfT | ThenT | ElseT
  | CommandT String
  deriving (Show, Eq)

tokChars :: [(Token, Char)]
charToks :: [(Char, Token)]
tokChars = [
  (LParenT, '('),
  (RParenT, ')'),
  (LBraceT, '{'),
  (RBraceT, '}'),
  (ColonT,  ':'),
  (CommaT,  ','),
  (DotT,    '.'),
  (AddT,    '+'),
  (SubT,    '-'),
  (MulT,    '*'),
  (SemiT,   ';'),
  (EqT,     '=')]
charToks = map swap tokChars

tokenName :: Token -> String
tokenName t = case t of
  NatT n -> "number " ++ show n
  VarT v -> "variable " ++ q v
  ArrT -> q "->"
  IfkT -> q "ifk"; IfT -> q "if"; ThenT -> q "then"; ElseT -> q "else"
  _ | Just c <- lookup t tokChars -> q [c]
    | otherwise -> error "No such token?!"
  where q = show

(<:>) :: Functor f => a -> f [a] -> f [a]
a <:> fas = fmap (a:) fas

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize ('-':'>':cs) = ArrT <:> tokenize cs
tokenize ('`':cs) -- TODO allow escaping?
  | (cmd, '`':cs') <- break (=='`') cs = CommandT cmd <:> tokenize cs'
tokenize (c:cs)
  | Just tok <- lookup c charToks = tok <:> tokenize cs
  | isSpace c = tokenize cs
tokenize (c:cs) | isAlpha c || c == '_' = tok <:> tokenize rest
  where validInVar c' = isAlphaNum c' || c' `elem` "'_"
        (name, rest) = span validInVar (c:cs)
        tok = case name of
          "ifk" -> IfkT; "if" -> IfT; "then" -> ThenT; "else" -> ElseT
          v -> VarT v
tokenize (c:cs) | c `elem` digits = tok <:> tokenize rest
  where digits = ['0'..'9']
        (lit, rest) = span (`elem` digits) (c:cs)
        tok = NatT (read lit)
tokenize cs = Left cs

-- Why isn't this in Earley?!
sepBy :: Prod r e t a -> Prod r e t b -> Prod r e t [a]
sepBy item sep =
  liftA2 (++) (many (item <* sep)) (maybeToList <$> optional item)

-- ugh, this is kinda hacky
data MixfixPiece
  = MPNone
  | MPSym Symbol
  | MPArgs [Expr]
  | MPParams [Symbol]
  deriving (Eq, Show)
exprG :: Grammar r (Prod r String Token Expr)
exprG = mdo
  let toSym (VarT v) = Just v
      toSym _ = Nothing
      toNat (NatT n) = Just n
      toNat _ = Nothing
      toCmd (CommandT cmd) = Just cmd
      toCmd _ = Nothing
      tok t = token t <?> tokenName t
  sym <- rule $ terminal toSym <?> "variable name"
  int <- rule $
    terminal toNat <|> negate <$> (tok SubT *> terminal toNat) <?> "int"
  compound <- rule . (<?> "compound") $
    tok LBraceT *>
    liftA2 (,) (sym <* tok ColonT) expr `sepBy` tok CommaT
    <* tok RBraceT
  progn <- rule . (<?> "progn") $
    tok LBraceT *> some (expr <* tok SemiT) <* tok RBraceT
  command <- rule $ terminal toCmd <?> "command"
  atom <- rule . asum $ [
    Var <$> sym, IntExpr <$> int,
    CompoundExpr <$> compound, Progn <$> progn,
    CommandExpr <$> command,
    tok LParenT *> expr <* tok RParenT]
  let tok' t   = Just (MPNone <$ tok t)
      mpSym    = Just $ MPSym <$> sym
      mpArgs   = Just $ MPArgs <$> expr `sepBy` tok CommaT <?> "argument list"
      mpParams = Just . fmap MPParams . (<?> "parameter list") $
        tok LParenT *> sym `sepBy` tok CommaT <* tok RParenT <|>
        pure <$> sym
      table = [
          [([mpSym, tok' EqT, Nothing], RightAssoc,
            \[Just (MPSym s), _, _] [e] -> Assign s e),
           ([mpParams, tok' ArrT, Nothing],
            RightAssoc, \[Just (MPParams params), _, _] [body] ->
              Lam Nothing params body),
           ([mpSym, mpParams, tok' ArrT, Nothing], RightAssoc,
            \[Just (MPSym s), Just (MPParams params), _, _] [body] ->
              Lam (Just s) params body),
           ([mpSym, mpParams, tok' EqT, Nothing], RightAssoc,
            \[Just (MPSym s), Just (MPParams params), _, _] [body] ->
              Assign s (Lam (Just s) params body)),
           ([tok' IfkT, Nothing, tok' DotT, mpSym, tok' ThenT, Nothing,
             tok' ElseT, Nothing], RightAssoc,
            \[_, _, _, Just (MPSym key), _, _, _, _] [cond, th, el] ->
              IfKeyExpr cond key th el),
           ([tok' IfT, Nothing, tok' ThenT, Nothing,
             tok' ElseT, Nothing], RightAssoc,
            \[_, _, _, _, _, _] [cond, th, el] -> IfExpr cond th el)],
          [([Nothing, tok' AddT, Nothing], LeftAssoc,
            \[_, _, _] [a, b] -> ArithExpr Add a b),
           ([Nothing, tok' SubT, Nothing], LeftAssoc,
            \[_, _, _] [a, b] -> ArithExpr Sub a b)],
          [([Nothing, tok' MulT, Nothing], LeftAssoc,
            \[_, _, _] [a, b] -> ArithExpr Mul a b)],
          [([Nothing, tok' DotT, mpSym], LeftAssoc,
            \[_, _, Just (MPSym key)] [e] -> ProjExpr key e),
           ([Nothing, tok' LParenT, mpArgs, tok' RParenT], LeftAssoc,
            \[_, _, Just (MPArgs args), _] [fn] -> App fn args)]
        ]
  expr <- mixfixExpressionSeparate table atom
  return expr

exprParser :: Parser String [Token] Expr
exprParser = parser exprG

-- A Left return contains a human-readable error message.
parseExpr :: [Token] -> Either String Expr
parseExpr tokens = case fullParses exprParser tokens of
  ([e], _) -> Right e
  ([], rep) -> Left $
    let ex = case expected rep of
               [] -> "EOF"
               [t] -> t
               ts -> "one of " ++ intercalate ", " ts
        found = case unconsumed rep of
                  [] -> "EOF"
                  tok:_ -> tokenName tok
    in "Parse error at token " ++ show (position rep) ++
       ": expected " ++ ex ++ "; found " ++ found
  (ps, _) -> Left $ "Ambiguous parse: could be any of " ++
    intercalate ", " (map show ps)

