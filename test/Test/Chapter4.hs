{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# LANGUAGE TypeApplications #-}

module Test.Chapter4
    ( chapter4
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Chapter4


chapter4 :: Spec
chapter4 = describe "Chapter4" $ do
    chapter4normal
    chapter4advanced

chapter4normal :: Spec
chapter4normal = describe "Chapter4Normal" $ do
    describe "Task2: Functor for Secret" $ do
        let trap = Trap "it's a trap"
        it "doen't affect trap" $
            fmap @(Secret String) @Bool not trap `shouldBe` trap
        it "change reward, same type" $
            fmap @(Secret String) @Bool not (Reward False) `shouldBe` Reward True
        it "change reward, other type" $
            fmap @(Secret String) @Int even (Reward 5) `shouldBe` Reward False
        it "change reward, other type" $
            fmap @(Secret String) @Int even (Reward 4) `shouldBe` Reward True
    describe "Task4: Applicative for Secret" $ do
        let trap :: Secret String Int
            trap = Trap "it's a trap"
        it "pure int" $
            pure @(Secret String) "x" `shouldBe` Reward "x"
        it "pure bool" $
            pure @(Secret String) False `shouldBe` Reward False
        it "trap <*> reward" $
            Trap "it's a trap" <*> Reward 42 `shouldBe` trap
        it "trap <*> trap" $
            Trap "it's a trap" <*> Trap "42" `shouldBe` trap
        it "reward <*> trap" $
            Reward not <*> Trap 42 `shouldBe` Trap 42
        it "reward <*> reward - same type" $
            Reward not <*> Reward True `shouldBe` (Reward False :: Secret String Bool)
        it "reward <*> reward" $
            Reward odd <*> Reward 42 `shouldBe` (Reward False :: Secret String Bool)
    describe "add two lists" $ do
        let list = Cons 1 (Cons 2 Empty)

        it "returns first list if second is empty" $ do
            addLists list Empty `shouldBe` list

        it "returns second list if first one is empty" $ do
            addLists Empty list `shouldBe` list

        it "returns united list" $ do
            addLists list list `shouldBe` Cons 1 (Cons 2 (Cons 1 (Cons 2 Empty)))

    describe "flatten list of lists" $ do
        it "single list lists" $ do
            concatL (Cons (Cons 1 (Cons 2 Empty)) Empty) `shouldBe` Cons 1 (Cons 2 Empty)

        it "when there are two lists and the first of them is empty" $ do
            concatL (Cons Empty (Cons (Cons 16 (Cons 42 Empty)) Empty)) `shouldBe` Cons 16 (Cons 42 Empty)

        it "two lists" $ do
            concatL (Cons (Cons 1 (Cons 2 Empty)) (Cons (Cons 3 (Cons 4 Empty)) Empty)) `shouldBe` Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty)))

        it "arbitrary amount of lists" $ do
            let list1 :: List Int
                list1 = Cons 420 (Cons 421 (Cons 422 Empty))
            let list2 :: List Int
                list2 = Cons 520 (Cons 521 (Cons 522 (Cons 523 Empty)))
            let list3 :: List Int
                list3 = Cons 620 (Cons 621 Empty)
            let input :: List (List Int)
                input = Cons list3 (Cons Empty (Cons list2 (Cons Empty (Cons list1 Empty))))
            let output :: List Int
                output = Cons 620 (Cons 621 (Cons 520 (Cons 521 (Cons 522 (Cons 523 (Cons 420 (Cons 421 (Cons 422 Empty))))))))

            concatL input `shouldBe` output
    describe "Task5: Applicative for list" $ do
        it "produces the multiplication of lists" $ do
            let fs :: List (Int -> Int)
                fs = Cons (+ 1) (Cons (+ 2) Empty)
            let vs :: List Int
                vs = Cons 5 (Cons 6 (Cons 7 Empty))

            fs <*> vs `shouldBe` Cons 6 (Cons 7 (Cons 7 (Cons 8 (Cons 8 (Cons 9 Empty)))))
    describe "Task6: Monad for Secret" $ do
        it "Trap" $ (Trap "aaar" >>= halfSecret) `shouldBe` Trap "aaar"
        it "Reward even" $ (Reward 42 >>= halfSecret) `shouldBe` Reward 21
        it "Reward odd" $ (Reward 11 >>= halfSecret) `shouldBe` Trap "it's a trap"

    describe "Task 9*" $ do
        let tree :: Tree Int
            tree = Node (Node (Leaf 4) 2 (Node (Leaf 6) 5 (Leaf 7))) 1 (Node (Node (Leaf 9) 8 (Leaf 10)) 3 (Leaf 11))
        describe "Functor" $ do
            it "maps a tree which consists of single leaf" $ do
                fmap (+3) (Leaf 17) `shouldBe` Leaf 20
            it "maps every element in a tree" $ do
                fmap (*2) tree `shouldBe` Node (Node (Leaf 8) 4 (Node (Leaf 12) 10 (Leaf 14))) 2 (Node (Node (Leaf 18) 16 (Leaf 20)) 6 (Leaf 22))
        describe "reverseTree" $ do
            it "leaves a leaf intact" $ do
                reverseTree (Leaf 3) `shouldBe` Leaf 3
            it "changes the leafs of the node but leaves value intact" $ do
                reverseTree (Node (Leaf 1) 2 (Leaf 3)) `shouldBe` Node (Leaf 3) 2 (Leaf 1)
            it "reverses all the tree recursively" $ do
                let reversedTree :: Tree Int
                    reversedTree = Node (Node (Leaf 11) 3 (Node (Leaf 10) 8 (Leaf 9))) 1 (Node (Node (Leaf 7) 5 (Leaf 6)) 2 (Leaf 4))

                reverseTree tree `shouldBe` reversedTree
        describe "treeToList" $ do
            it "converts leaf to a singleton list" $ do
                treeToList (Leaf 98) `shouldBe` Cons 98 Empty
            it "converts a single node to a three element list" $ do
                treeToList (Node (Leaf 76) 23 (Leaf 15)) `shouldBe` Cons 23 (Cons 76 (Cons 15 Empty))
            it "converts tree to the list" $ do
                treeToList tree `shouldBe` Cons 1 (Cons 2 (Cons 4 (Cons 5 (Cons 6 (Cons 7 (Cons 3 (Cons 8 (Cons 9 (Cons 10 (Cons 11 Empty))))))))))

chapter4advanced :: Spec
chapter4advanced = describe "Chapter4Advanced" $
    describe "Task 8*: Before the Final Boss" $ do
        it "Nothing - Nothing" $ andM Nothing Nothing `shouldBe` Nothing
        it "Nothing - Just" $ andM Nothing (Just True) `shouldBe` Nothing
        it "Just True - Nothing" $ andM (Just True) Nothing `shouldBe` Nothing
        it "Just False - Nothing" $ andM (Just False) Nothing `shouldBe` Just False
        it "Just - Just : False" $ andM (Just True) (Just False) `shouldBe` Just False
        it "Just - Just : True" $ andM (Just True) (Just True) `shouldBe` Just True

halfSecret :: Int -> Secret String Int
halfSecret n
    | even n = Reward (div n 2)
    | otherwise = Trap "it's a trap"
