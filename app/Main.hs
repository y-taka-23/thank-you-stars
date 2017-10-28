module Main (main) where

import Utils.ThankYouStars.GitHub
import Utils.ThankYouStars.Package

import qualified Data.Set          as S
import           System.Exit       ( die )
import           System.Directory  ( getHomeDirectory )
import           System.FilePath   ( joinPath )

main :: IO ()
main = do
    homeDir <- getHomeDirectory
    let tokenFile = joinPath [homeDir, ".thank-you-stars.json"]
    eToken <- readToken tokenFile
    case eToken of
        Left  _ -> do
            die ("Cannot parse " ++ show tokenFile ++ " into a token")
        Right token -> do
            db     <- readStackIndex
            cabals <- getCabalFiles
            descs  <- mapM readCabalFile $ S.toList cabals
            let repos = S.unions $ map (dependentRepos db) descs
            mapM_ (starAction token) repos

starAction :: Token -> GitHubRepo -> IO ()
starAction token ghr = do
    eResult <- starRepo token ghr
    case eResult of
        Left  _  -> putStrLn $ "Error    " ++ show ghr
        Right () -> putStrLn $ "Starred! " ++ show ghr
