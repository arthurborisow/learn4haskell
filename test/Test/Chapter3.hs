module Test.Chapter3
    ( chapter3
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Chapter3


chapter3 :: Spec
chapter3 = describe "Chapter3" $ do
    describe "Chapter3Normal" $ do
        describe "nextDay" $ do
            it "returns Tuesday for Monday" $ nextDay Monday `shouldBe` Tuesday
            it "returns Wednesday for Tuesday" $ nextDay Tuesday `shouldBe` Wednesday
            it "returns Thursday or Wednesday" $ nextDay Wednesday `shouldBe` Thursday
            it "returns Friday for Thursday" $ nextDay Thursday `shouldBe` Friday
            it "returns Saturday for Friday" $ nextDay Friday `shouldBe` Saturday
            it "returns Sunday for Saturday" $ nextDay Saturday `shouldBe` Sunday
            it "returns Monday for Sunday" $ nextDay Sunday `shouldBe` Monday
        describe "daysToParty" $ do
            it "returns 4 for Monday" $ daysToParty Monday `shouldBe` 4
            it "returns 3 for Tuesday" $ daysToParty Tuesday `shouldBe` 3
            it "returns 2 for Wednesday" $ daysToParty Wednesday `shouldBe` 2
            it "returns 1 for Thursday" $ daysToParty Thursday `shouldBe` 1
            it "returns 0 for Friday" $ daysToParty Friday `shouldBe` 0
            it "returns 6 for Saturday" $ daysToParty Saturday `shouldBe` 6
            it "returns 5 for Sunday" $ daysToParty Sunday `shouldBe` 5
    describe "Chapter3Advanced" $ it "" $ True `shouldBe` True
