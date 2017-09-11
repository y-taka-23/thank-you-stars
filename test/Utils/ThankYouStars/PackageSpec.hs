{-# LANGUAGE OverloadedStrings  #-}
module Utils.ThankYouStars.PackageSpec (spec) where

import Utils.ThankYouStars.GitHub
import Utils.ThankYouStars.Package

import Data.Set                ( fromList )
import Data.Map                ( empty )
import Distribution.Hackage.DB ( readHackage )
import Distribution.Package
import Test.Hspec

spec :: Spec
spec = do
    describe "getThisPackageName" $ do
        it "returns the name of project root directory" $ do
            getThisPackageName `shouldReturn`
                PackageName { unPackageName = "thank-you-stars" }

    describe "allBuildDepends" $ do
        it "returns all packages which are listed in build-depends" $ do
            desc <- readCabalFile "thank-you-stars.cabal"
            allBuildDepends desc `shouldBe` fromList (map PackageName [
                  "aeson"
                , "base"
                , "bytestring"
                , "Cabal"
                , "containers"
                , "directory"
                , "filepath"
                , "hackage-db"
                , "hspec"
                , "req"
                , "split"
                , "text"
                , "thank-you-stars"
                ])

    describe "lookupRepo" $ do
        it "extracts source-repository from the local Hackage DB" $ do
            db <- readHackage
            lookupRepo (PackageName { unPackageName = "hspec"}) db
                `shouldBe` Just GitHubRepo { owner = "hspec", repo = "hspec" }
        it "returns Nothing if the package is not hosted on GitHub" $ do
            db <- readHackage
            lookupRepo (PackageName { unPackageName = "base"}) db
                `shouldBe` Nothing
        it "returns Nothing if the package is not in the local DB" $ do
            lookupRepo (PackageName { unPackageName = "hspec"}) empty
                `shouldBe` Nothing
