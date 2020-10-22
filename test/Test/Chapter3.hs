module Test.Chapter3
    ( chapter3
    ) where

import Test.Hspec (Spec, describe, it, specify, shouldBe)

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
    describe "Chapter3Advanced" $ do
        describe "Fighting" $ do
            describe "buffing" $ do
                specify "can increase health" $ do
                    let fighter = Knight (Health 100) (Attack 300) (Defense 200) []

                    buff fighter (PotionAction (Health 13)) `shouldBe` Alive (fighter { knightHealth = knightHealth fighter `append` Health 13 })

            specify "the first fighter who hits with more damage than opponent's health wins" $ do
                let fighter1 = Knight (Health 200) (Attack 300) (Defense 200) [AttackAction]
                let fighter2 = Knight (Health 100) (Attack 300) (Defense 0) [AttackAction]

                letsFight fighter1 fighter2 `shouldBe` FirstWinner fighter1

            specify "when first can't kill with one hit but second can the second is a winner" $ do
                let fighter1 = Knight (Health 200) (Attack 10) (Defense 0) [AttackAction]
                let fighter2 = Knight (Health 100) (Attack 300) (Defense 300) [AttackAction]

                letsFight fighter1 fighter2 `shouldBe` SecondWinner fighter2

            specify "fighters of different types" $ do
                let fighter1 = Knight (Health 200) (Attack 300) (Defense 200) [AttackAction]
                let fighter2 = Monster (Health 100) (Attack 300) [HitAction]

                letsFight fighter1 fighter2 `shouldBe` FirstWinner fighter1

            specify "when second has to win but first drinks health potion all the time" $ do
                let fighter1 = Knight (Health 100) (Attack 10) (Defense 0) [AttackAction, PotionAction (Health 50)]
                let fighter2 = Monster (Health 100) (Attack 25) [HitAction]

                letsFight fighter1 fighter2 `shouldBe` FirstWinner fighter1

            specify "monsters can run away" $ do
                let fighter1 = Monster (Health 100) (Attack 100) [HitAction, HitAction, HitAction, HitAction, RunAction]
                let fighter2 = Knight (Health 100) (Attack 10) (Defense 80) [AttackAction]

                letsFight fighter1 fighter2 `shouldBe` SecondWinner (fighter2 { knightHealth = Health 20 })