module Utils.ThankYouStars.PackageSpec (spec) where

import Utils.ThankYouStars.GitHub
import Utils.ThankYouStars.Package

import qualified Data.Set         as S
import           System.Directory ( getCurrentDirectory )
import           System.FilePath  ( joinPath )
import           Test.Hspec

spec :: Spec
spec = do
    describe "getCabalFiles" $ do
        it "returns the set of all .cabal files in the subdirectories" $ do
            curr <- getCurrentDirectory
            getCabalFiles `shouldReturn` S.fromList [
                  joinPath [curr, "thank-you-stars.cabal"]
                , joinPath [curr, "test", "Fixture", "dummy.cabal"]
                ]

    -- TODO: too fragile. mock the stack DB
    describe "dependentRepos" $ do
        it "returns the set of all GitHub repos listed in build-depends" $ do
            db   <- readStackIndex
            desc <- readCabalFile "thank-you-stars.cabal"
            dependentRepos db desc `shouldBe` S.fromList [
                  GitHubRepo "bos" "aeson"
                , GitHubRepo "haskell" "bytestring"
                , GitHubRepo "haskell" "cabal"
                , GitHubRepo "haskell" "containers"
                , GitHubRepo "haskell" "directory"
                , GitHubRepo "haskell" "filepath"
                , GitHubRepo "peti" "hackage-db"
                , GitHubRepo "hspec" "hspec"
                , GitHubRepo "mrkkrp" "req"
                , GitHubRepo "byorgey" "split"
                , GitHubRepo "bos" "text"
                ]
