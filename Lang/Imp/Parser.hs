-------------------------------------------------------------------------------
-- Parser for the Imp language
-- https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form
-------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Lang.Imp.Parser where

import Lang.Parser.Combinators
import Lang.Imp.Ast
import Control.Applicative hiding (Const)

todo :: a
todo = error "TODO"

-- cSeq = cmd { ";" cmd }
cSeqP :: Parser Cmd
cSeqP = do 
    cmd <- cmdP
    cmds <- many (symbol ";" *> cmdP)
    pure $ CSeq (cmd:cmds)

-- cmd = cSkip | cAssign | cIfThEl | cWhile
cmdP :: Parser Cmd
cmdP = cSkipP 
   <|> cAssignP 
   <|> cIfThElP 
   <|> cWhileP

-- cSkip = "skip"
cSkipP :: Parser Cmd
cSkipP = symbol "skip" *> pure CSkip

-- cAssign = ident ":=" arithExpr
cAssignP :: Parser Cmd
cAssignP = do
    name <- token ident
    symbol ":="
    expr <- arithExprP
    pure (CAssign name expr)


-- cIfThEl = "if" boolExpr "then" cseq "else" cseq "endif"
cIfThElP :: Parser Cmd
cIfThElP = do
    symbol "if"
    bExpr <- boolExprP
    symbol "then"
    thCmd <- cSeqP
    symbol "else"
    elCmd <- cSeqP
    symbol "endif"
    pure (CIfThEl bExpr thCmd elCmd)


-- cWhile = "while" boolExpr "do" cseq "endwhile"
cWhileP :: Parser Cmd
cWhileP = do
    symbol "while"
    bExpr <- boolExprP
    symbol "do"
    cmd <- cSeqP
    symbol "endwhile"
    pure (CWhile bExpr cmd)


-- arithExpr = const | var | arithBinOp
arithExprP :: Parser (Expr Integer)
arithExprP = constP <|> varP <|> arithBinOpP


-- constP :: Parser (Expr Integer)
-- constP = do 
--     ds <- some digit
--     let numbers = map (fromIntegral . digitToInt) ds
--     let val = foldl (\acc d -> acc * 10 + d) 0 numbers
--     pure (AConst val)

-- nat = <digit> {<digit>}
natP :: Parser Integer
natP = fmap read (some digit)


-- const = nat | "-" nat
constP :: Parser (Expr Integer)
constP = fmap AConst (natP <|> (pure negate <* char '-' <*> natP))


-- var = ident
varP :: Parser (Expr Integer)
varP = fmap AVar (token ident)


-- arithBinOp = "(" arithExpr arithOp arithExpr ")"
arithBinOpP :: Parser (Expr Integer)
arithBinOpP = do
    symbol "("
    l <- arithExprP
    op <- arithOpP
    r <- arithExprP
    symbol ")"
    pure (case op of
        APlusOp -> APlus l r
        AMinusOp -> AMinus l r
        AMulOp -> AMul l r
        ADivOp -> ADiv l r
        AModOp -> AMod l r)

data AOp = APlusOp | AMinusOp | AMulOp | ADivOp | AModOp

-- arithOp = "+" | "-" | "*" | "/" | "%"
arithOpP :: Parser AOp
arithOpP = symbol "+" *> pure APlusOp
       <|> symbol "-" *> pure AMinusOp
       <|> symbol "*" *> pure AMulOp
       <|> symbol "/" *> pure ADivOp
       <|> symbol "%" *> pure AModOp


-- boolExpr = bNotExpr | bAndEpr | relBExpr
boolExprP :: Parser (Expr Bool)
boolExprP = bNotExprP <|> bOpExprP <|> relBExprP

-- bNotExpr = "not" boolExpr
bNotExprP :: Parser (Expr Bool)
bNotExprP = do
    string "not"
    bExpr <- boolExprP
    pure (BNot bExpr)


-- bOpExprP = "(" boolExpr bOp boolExpr ")"
bOpExprP :: Parser (Expr Bool)
bOpExprP = do
    symbol "("
    l <- boolExprP
    op <- bOpP
    r <- boolExprP
    symbol ")"
    pure (case op of
        BAndOp -> BAnd l r
        BOrOp -> BOr l r)

data BOp = BAndOp | BOrOp

-- bOp = "&&" | "||"
bOpP :: Parser BOp
bOpP = symbol "&&" *> pure BAndOp
   <|> symbol "||" *> pure BOrOp
   

-- relBExpr = "(" arithExpr relOp arithExpr ")"
relBExprP :: Parser (Expr Bool)
relBExprP = do
    symbol "("
    l <- arithExprP
    op <- relOpP
    r <- arithExprP
    symbol ")"
    case op of
        REqOp -> pure $ REq l r
        RLtOp -> pure $ RLt l r
        RLtEqOp -> pure $ BOr (RLt l r) (REq l r) 
        -- a <= b is syntactic sugar and expressed as: a < b || a == b


data ROp = REqOp | RLtOp | RLtEqOp


-- arithExpr = "<=" | "<" | "=" 
relOpP :: Parser ROp
relOpP = symbol "<=" *> pure RLtEqOp -- order is relevant here
     <|> symbol "<" *> pure RLtOp
     <|> symbol "=" *> pure REqOp
      


-- ident = <lower> {<alphaNum>}
ident :: Parser String
ident = do
  c <- lower
  cs <- many alphaNum
  pure (c:cs)

-- Entry point for the parser
parseImp :: String -> Either String Cmd
parseImp input = case parse cSeqP input of
    Nothing -> Left "Parse error"
    Just (cmd, "") -> Right cmd
    Just (_, rest) -> Left ("Parse error: " ++ rest)
