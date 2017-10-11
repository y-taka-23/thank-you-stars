{-# LANGUAGE OverloadedStrings  #-}
module Utils.ThankYouStars.PackageSpec (spec) where

import Utils.ThankYouStars.GitHub
import Utils.ThankYouStars.Package

import qualified Data.Set                as S
import qualified Data.Map                as M
import           Distribution.Hackage.DB ( readHackage )
import           Distribution.Package
import           Distribution.Version
import           Test.Hspec

spec :: Spec
spec = do
    describe "getThisPackageName" $ do
        it "returns the name of project root directory" $ do
            getThisPackageName `shouldReturn`
                PackageName { unPackageName = "thank-you-stars" }

    describe "allBuildDepends" $ do
        it "returns all packages which are listed in build-depends" $ do
            desc <- readCabalFile "thank-you-stars.cabal"
            allBuildDepends desc `shouldBe` S.fromList (map PackageName [
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
                , "safe"
                , "split"
                , "text"
                , "thank-you-stars"
                ])

    describe "lookupRepo" $ do
        it "extracts source-repository from the package description" $ do
            desc <- readCabalFile "thank-you-stars.cabal"
            let ver = Version [] []
                db  = M.singleton "thank-you-stars" (M.singleton ver desc)
            lookupRepo (PackageName { unPackageName = "thank-you-stars" }) db
                `shouldBe` Just GitHubRepo {
                      owner = "y-taka-23"
                    , repo  = "thank-you-stars"
                    }
        it "returns Nothing if the package is not found" $ do
            lookupRepo (PackageName { unPackageName = "hspec" }) M.empty
                `shouldBe` Nothing
