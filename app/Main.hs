module Main (main) where

import Utils.ThankYouStars.GitHub
import Utils.ThankYouStars.Package

import           Control.Exception ( SomeException, catch )
import           Data.Maybe        ( catMaybes )
import qualified Data.Set          as S
import           System.Exit       ( die )
import           System.Directory  ( getHomeDirectory )
import           System.FilePath   ( joinPath, (<.>) )

main :: IO ()
main = do
    homeDir <- getHomeDirectory
    thisPkg <- getThisPackageName
    let tokenFile = joinPath [homeDir, ".thank-you-stars.json"]
        cabalFile = unPackageName thisPkg <.> "cabal"
    eToken <- readToken tokenFile
    case eToken of
        Left  _ -> do
            die ("Cannot parse " ++ show tokenFile ++ " into a token")
        Right token  -> do
            desc <- readCabalFile cabalFile
            db   <- readStackIndex
            let pkgs   = S.delete thisPkg $ allBuildDepends desc
                mRepos = S.map (flip lookupRepo $ db) pkgs
                repos  = catMaybes $ S.toList mRepos
            mapM_ (starAction token) repos

starAction :: Token -> GitHubRepo -> IO ()
starAction token ghr = do
    eResult <- starRepo token ghr
    case eResult of
        Left  _  -> putStrLn $ "Error    " ++ show ghr
        Right () -> putStrLn $ "Starred! " ++ show ghr
