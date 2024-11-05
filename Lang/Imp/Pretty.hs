-------------------------------------------------------------------------------
-- Pretty printer for the Imp language
-------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
module Lang.Imp.Pretty where
import Lang.Imp.Ast

-- | Pretty print an expression
ppExpr :: Expr a -> String
ppExpr (BNot e) = "not" ++ ppExpr e
ppExpr (BAnd e1 e2) = binaryWithParens "&&" e1 e2
ppExpr (BOr e1 e2) = binaryWithParens "||" e1 e2

ppExpr (REq e1 e2) = binaryWithParens "=" e1 e2
ppExpr (RLt e1 e2) = binaryWithParens "<" e1 e2

ppExpr (AConst n) = show n
ppExpr (APlus e1 e2) = binaryWithParens "+" e1 e2
ppExpr (AMinus e1 e2) = binaryWithParens "-" e1 e2
ppExpr (AMul e1 e2) = binaryWithParens "*" e1 e2
ppExpr (ADiv e1 e2) = binaryWithParens "/" e1 e2
ppExpr (AMod e1 e2) = binaryWithParens "%" e1 e2
ppExpr (AVar x) = x

binaryWithParens :: String -> Expr a -> Expr a -> String
binaryWithParens op e1 e2 = parens (ppExpr e1 ++ " " ++ op ++ " " ++ ppExpr e2)

parens :: String -> String
parens s = "(" ++ s ++ ")"

-- | Pretty print a command
ppCmd :: Cmd -> String
ppCmd cmd = ppCmd' cmd ""

ppCmd' :: Cmd -> String -> String
ppCmd' CSkip             ind = ind ++ "skip"
ppCmd' (CAssign x e)     ind = ind ++ x ++ " := " ++ ppExpr e 
ppCmd' (CIfThEl e c1 c2) ind = ind ++ "if " ++ ppExpr e ++ " then\n" ++ ppCmd' c1 (more ind)  ++ "\n" ++ ind ++ "else\n" ++ ppCmd' c2 ind ++ "\n" ++ ind ++ "endif"
ppCmd' (CWhile e c)      ind = ind ++ "while " ++ ppExpr e ++ " do\n" ++ ppCmd' c (more ind) ++ "\n" ++ ind ++"endwhile"
ppCmd' (CSeq [])         _   = ""
ppCmd' (CSeq [c])        ind = ppCmd' c ind
ppCmd' (CSeq (c:cs))     ind = ppCmd' c ind ++ ";\n" ++ ppCmd' (CSeq cs) ind

more :: String -> String
more ind = "  " ++ ind