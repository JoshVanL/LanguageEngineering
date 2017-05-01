import WhileParser

main :: IO ()
main = hspec $ do
    sDynamicSpec

sDynamicSpec :: Spec
sDynamicSpec = do
    describe "s_dynamic" $ do
        it "skips" $ do
            s_dynamic Skip testState "x" `shouldBe` 1
            s_dynamic Skip testState "y" `shouldBe` 2
            s_dynamic Skip testState "z" `shouldBe` 3

        it "assigns" $ do
            s_dynamic (Ass "x" (N 5)) testState "x" `shouldBe` 5
            s_dynamic (Ass "x" (N 5)) testState "y" `shouldBe` 2
            s_dynamic (Ass "x" (N 5)) testState "z" `shouldBe` 3

        it "performs s1 if IF predicate is true" $ do
            s_dynamic (If TRUE (Ass "x" (N 5)) (Ass "x" (N 0))) testState "x" `shouldBe` 5

        it "performs s2 if IF predicate is false" $ do
            s_dynamic (If FALSE (Ass "x" (N 5)) (Ass "x" (N 0))) testState "x" `shouldBe` 0

        it "does composition" $ do
            s_dynamic (Comp (Ass "x" (N 5)) (Ass "y" (N 6))) testState "x" `shouldBe` 5
            s_dynamic (Comp (Ass "x" (N 5)) (Ass "y" (N 6))) testState "y" `shouldBe` 6
            s_dynamic (Comp (Ass "x" (N 5)) (Ass "y" (N 6))) testState "z" `shouldBe` 3

        it "does not perform the body of the while loop of the condition is false" $ do
            s_dynamic (While FALSE (Ass "x" (N 5))) testState "x" `shouldBe` 1

        it "does while loops" $ do
            s_dynamic (While (Neg (Eq (V "z") (N 0))) (Ass "z" (Sub (V "z") (N 1)))) testState "z" `shouldBe` 0

        it "correctly evaulates program" $ do
            s_dynamic scopeStm emptyState "y" `shouldBe` 6

        it "evaulates fac loop" $ do
            s_dynamic facLoop (update "x" 1 emptyState) "y" `shouldBe` 1
            s_dynamic facLoop (update "x" 2 emptyState) "y" `shouldBe` 2
            s_dynamic facLoop (update "x" 3 emptyState) "y" `shouldBe` 6
            s_dynamic facLoop (update "x" 4 emptyState) "y" `shouldBe` 24
            s_dynamic facLoop (update "x" 5 emptyState) "y" `shouldBe` 120

        it "evaulates with self-recursion" $ do
            s_dynamic facCall (update "x" 1 emptyState) "y" `shouldBe` 1
            s_dynamic facCall (update "x" 2 emptyState) "y" `shouldBe` 2
            s_dynamic facCall (update "x" 3 emptyState) "y" `shouldBe` 6
            s_dynamic facCall (update "x" 4 emptyState) "y" `shouldBe` 24
            s_dynamic facCall (update "x" 5 emptyState) "y" `shouldBe` 120

        it "evaulates with mutual-recursion" $ do
            s_dynamic mutualEvenOdd (update "x" 0 emptyState) "y" `shouldBe` 0
            s_dynamic mutualEvenOdd (update "x" 1 emptyState) "y" `shouldBe` 1
            s_dynamic mutualEvenOdd (update "x" 2 emptyState) "y" `shouldBe` 0
            s_dynamic mutualEvenOdd (update "x" 3 emptyState) "y" `shouldBe` 1
            s_dynamic mutualEvenOdd (update "x" 4 emptyState) "y" `shouldBe` 0
            s_dynamic mutualEvenOdd (update "x" 10 emptyState) "x" `shouldBe` 0
