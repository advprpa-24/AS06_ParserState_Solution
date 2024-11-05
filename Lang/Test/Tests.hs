{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Test.Hspec ( hspec, describe, it, shouldBe, shouldContain, Spec )
import Lang.Imp.Ast
import Lang.Imp.Interpreter
import Lang.Imp.Parser
import qualified Data.Map.Strict as M

testExec :: Cmd -> State -> State 
testExec cmd inputState = either error fst (runState (exec cmd) inputState)

evalSpec :: Spec
evalSpec = 
  describe "eval" $ do 
    it "runExpr (eval (AConst 1)) empty == Right 1" $
      runExpr (eval (AConst 1)) emptyState `shouldBe` Right 1

    it "runExpr (eval (AVar \"a\")) empty == Left \"variable not found: a\"" $
      runExpr (eval (AVar "a")) emptyState `shouldBe` Left "variable not found: a"
    it "runExpr (eval (AVar \"a\")) M.fromList [(\"x\", 5)] == Right 5" $
      runExpr (eval (AVar "a")) (M.fromList [("a", 5)]) `shouldBe` Right 5

execSpec :: Spec
execSpec = 
  describe "exec" $ do 
    it "exec CSkip emptyState == emptyState" $
      runState (exec CSkip) emptyState `shouldBe` Right (emptyState, ())
    it "M.toList (exec (CAssign \"a\" (AConst 2) emptyState) == [(\"a\",2)]" $
      M.toList endState `shouldBe` [("a",2)]
      where endState = testExec (CAssign "a" (AConst 2)) emptyState


{-
Factorial:

z := x;
y := 1;
while (not (z = 0))
  y := y * z;
  z := z - 1
endwhile
-}
fact :: Cmd
fact = 
  CSeq [
    CAssign "z" (AVar "x"),
    CAssign "y" (AConst 1),
    CWhile (BNot (REq (AVar "z") (AConst 0))) (CSeq [
      CAssign "y" (AMul (AVar "y") (AVar "z")),
      CAssign "z" (AMinus (AVar "z") (AConst 1))
    ])
  ]

factSpec :: Spec
factSpec = describe "fact" $ do 
    -- fact [x=5] ~> y=120
    it "fact [\"x\" = 5)] == [\"y\" = 120]" $
      M.lookup "y" endState `shouldBe` Just 120 
      where endState = testExec fact (M.fromList [("x", 5)])
    

{-
Quotient remainder:

q := 0;
r := m;
while (n <= r)
  q := q + 1;
  r := r - n
endwhile
-}
intDiv :: Cmd
intDiv = 
  CSeq [
    CAssign "q" (AConst 0),
    CAssign "r" (AVar "m"),
    CWhile (BOr (RLt (AVar "n") (AVar "r")) (REq (AVar "n") (AVar "r"))) (CSeq [
        CAssign "q" (APlus (AVar "q") (AConst 1)),
        CAssign "r" (AMinus (AVar "r") (AVar "n"))
    ])
  ]

intDivSpec :: Spec
intDivSpec = describe "intDiv" $ do 
    -- intDiv 5 2 == q_uotient=2 r_emainder=1
    it "intDiv [\"m\" = 5, \"n\" = 2)]) ~> [\"q\" = 2, \"r\" = 1]" $
       M.toList endState `shouldContain` [("q", 2), ("r", 1)] 
      where endState = testExec intDiv (M.fromList [("m", 5), ("n", 2)])

{-
function gcd(a, b)
    while b ≠ 0
        t := b
        b := a mod b
        a := t
    return a
-}
gcd' :: Cmd
gcd' = CSeq [
  CWhile (BNot (REq (AVar "b") (AConst 0))) (CSeq [
        CAssign "t" (AVar "b"),
        CAssign "b" (AMod (AVar "a") (AVar "b")),
        CAssign "a" (AVar "t")
    ])
  ]

gcdSpec :: Spec
gcdSpec = describe "gcd'" $ do 
    it "gcd' [\"a\" = 252, \"b\" = 105] -> [\"a\" = 21]" $
      M.lookup "a" endState `shouldBe` Just 21 
     where endState = testExec gcd' (M.fromList [("a", 252), ("b", 105)])


parseSpec :: Spec
parseSpec = describe "Parser'" $ do 
   it "parseImp \"x := 5\" == Right (CSeq [CAssign \"x\" (AConst 5)])" $
     parseImp "x := 5" `shouldBe` Right (CSeq [CAssign "x" (AConst 5)])
   it "parseImp \"x := 5; y := 6\" == Right (CSeq [CAssign \"x\" (AConst 5), CAssign \"y\" (AConst 6)])" $
      parseImp "x := 5; y := 6" `shouldBe` Right (CSeq [CAssign "x" (AConst 5), CAssign "y" (AConst 6)])


main :: IO ()
main = hspec $ do
  describe "1. Evaluation" $ do
    evalSpec
  describe "2. Execution" $ do
    execSpec
  describe "3. Example programs" $ do
    factSpec
    intDivSpec
    gcdSpec
  describe "4. Parser tests" $ do
    parseSpec
   
    