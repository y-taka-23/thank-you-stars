{-# LANGUAGE OverloadedStrings  #-}
module Utils.ThankYouStars.GitHubSpec (spec) where

import Utils.ThankYouStars.GitHub

import Test.Hspec

spec :: Spec
spec = do
    describe "readToken" $ do
        it "parses a JSON into a Token" $ do
            readToken "test/Fixture/valid-token.json"
                `shouldReturn` Right (Token { unToken = "test" })
        it "returns an error message in the case of parse failure" $ do
            readToken "test/Fixture/invalid-token.json"
                `shouldReturn` Left "Error in $: key \"token\" not present"
