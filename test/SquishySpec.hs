{-# LANGUAGE OverloadedStrings #-}

module SquishySpec where

import Test.Hspec
import Squishy
import Squishy.Types
import Squishy.Parser
import Squishy.NameResolution

spec :: Spec
spec = do
  describe "isValidExpr" $ do
    it "should work" $ do
      isValidExpr (Prim True) `shouldBe` True
      isValidExpr (Let "x" (Prim True) (Var "x" 0)) `shouldBe` True
      isValidExpr (Let "x" (Prim True) (Prim True)) `shouldBe` True
      isValidExpr (Let "x" (Let "y" (Prim True) (Var "x" 0)) (Prim True)) `shouldBe` True
      isValidExpr (Let "x" (Prim True) (Let "y" (Prim True) (Var "x" 1))) `shouldBe` True

      isValidExpr (Var "x" 0) `shouldBe` False
      isValidExpr (Var "x" 1) `shouldBe` False
      isValidExpr (Let "x" (Prim True) (Var "x" 1)) `shouldBe` False
      isValidExpr (Let "x" (Var "x" 0) (Prim True)) `shouldBe` False
      isValidExpr (Let "x" (Var "x" 1) (Prim True)) `shouldBe` False
      isValidExpr (Let "x" (Let "y" (Prim True) (Var "x" 1)) (Prim True)) `shouldBe` False
      isValidExpr (Let "x" (Prim True) (Let "x" (Prim True) (Var "x" 2))) `shouldBe` False

  describe "reduce" $ do
    it "should work" $ do
      reduce (Prim True) `shouldBe` Prim True
      reduce (Let "x" (Prim True) (Prim True)) `shouldBe` Prim True
      reduce (Let "x" (Prim True) (Var "x" 0)) `shouldBe` Prim True
      reduce (Let "x" (Prim True) (Var "x" 0)) `shouldBe` Prim True
      reduce (Let "x" (Prim True) (Let "y" (Var "x" 0) (Prim True))) `shouldBe` Prim True
      reduce (Let "x" (Prim True) (Let "y" (Prim False) (Var "x" 0))) `shouldBe` Prim True
      reduce (Let "x" (Prim True) (Let "y" (Prim False) (Var "y" 1))) `shouldBe` Prim False

  describe "printABT" $ do
    it "should work" $ do
      printABT (Prim True) `shouldBe` "True"
      printABT (Let "x" (Prim True) (Let "y" (Prim False) (Var "y" 1)))
        `shouldBe` "let x = True in\nlet y = False in\ny"

  describe "parseAST" $ do
    it "should work" $ do
      parseAST "True" `shouldBe` Right (Prim True)
      parseAST "let foo = True in False"
        `shouldBe` Right (Let "foo" (Prim True) (Prim False))
      parseAST "let x = True in\nlet y = False in y"
        `shouldBe` Right (Let "x" (Prim True) (Let "y" (Prim False) (Var "y" ())))
    describe "with parens" $ do
      it "should still work" $ do
        parseAST "(True)" `shouldBe` Right (Prim True)
        parseAST "( True )" `shouldBe` Right (Prim True)
        parseAST "( (True) )" `shouldBe` Right (Prim True)
        parseAST "let x = (let y = False in y) in x"
          `shouldBe` Right (Let "x" (Let "y" (Prim False) (Var "y" ())) (Var "x" ()))

  describe "resolveNames" $ do
    it "should work for ASTs without binding" $ do
      resolveNames       (Prim True)
        `shouldBe` Right (Prim True)

      resolveNames       (Prim True)
        `shouldBe` Right (Prim True)

    it "should work for ASTs with binding" $ do
      resolveNames       (Let "x" (Prim True) (Let "y" (Prim False) (Var "x" ())))
        `shouldBe` Right (Let "x" (Prim True) (Let "y" (Prim False) (Var "x" 0)))

      resolveNames       (Let "x" (Prim True) (Let "y" (Prim False) (Var "y" ())))
        `shouldBe` Right (Let "x" (Prim True) (Let "y" (Prim False) (Var "y" 1)))

      resolveNames       (Let "x" (Let "y" (Prim False) (Var "y" ())) (Var "x" ()))
        `shouldBe` Right (Let "x" (Let "y" (Prim False) (Var "y" 0)) (Var "x" 0))

    it "should work for ASTs with binding and shadowing" $ do
      resolveNames       (Let "x" (Prim True) (Let "x" (Prim False) (Var "x" ())))
        `shouldBe` Right (Let "x" (Prim True) (Let "x" (Prim False) (Var "x" 1)))
