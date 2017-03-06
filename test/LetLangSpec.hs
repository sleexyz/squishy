{-# LANGUAGE OverloadedStrings #-}

module LetLangSpec where

import Test.Hspec
import LetLang

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
        `shouldBe` "let x = { True } in { let y = { False } in { y } }"

  describe "parseABT" $ do
    it "should work" $ do
      parseABT "True" `shouldBe` Right (Prim True)
      parseABT "let foo = { True } in { False }" 
        `shouldBe` Right (Let "foo" (Prim True) (Prim False))
      parseABT "let x = { True } in { let y = { False } in { y } }"
        `shouldBe` Right (Let "x" (Prim True) (Let "y" (Prim False) (Var "y" 1))) 
