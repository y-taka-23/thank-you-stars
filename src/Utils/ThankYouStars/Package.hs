{-# LANGUAGE LambdaCase #-}

module Utils.ThankYouStars.Package (
      getThisPackageName
    , allBuildDepends
    , readHackage
    , lookupRepo
    , readCabalFile
    , unPackageName
    ) where

import Utils.ThankYouStars.GitHub

import           Control.Monad                         ( (<$!>) )
import           Data.List                             ( isInfixOf )
import           Data.List.Split                       ( splitOneOf )
import qualified Data.Map                              as M
import qualified Data.Set                              as S
import           Data.Maybe
import           Distribution.Hackage.DB               ( Hackage, readHackage )
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse ( readPackageDescription )
import           Distribution.Verbosity                ( normal )
import           Safe                                  ( headMay )
import           System.Directory                      ( getCurrentDirectory )
import           System.Environment                    ( getArgs )
import           System.FilePath                       ( takeBaseName )

allBuildDepends :: GenericPackageDescription -> S.Set PackageName
allBuildDepends desc =
    let ls = map (libBuildInfo . condTreeData) . maybeToList . condLibrary   $ desc
        es = map (buildInfo          . condTreeData . snd) . condExecutables $ desc
        ts = map (testBuildInfo      . condTreeData . snd) . condTestSuites  $ desc
        bs = map (benchmarkBuildInfo . condTreeData . snd) . condBenchmarks  $ desc
    in  S.fromList . concatMap depends $ ls ++ es ++ ts ++ bs

depends :: BuildInfo -> [PackageName]
depends = map toPackageName . targetBuildDepends

getThisPackageName :: IO PackageName
getThisPackageName = headMay <$!> getArgs >>= \case
  Nothing            -> fromCurrentDirectory
  Just specifiedName -> return $ PackageName specifiedName
  where
    fromCurrentDirectory :: IO PackageName
    fromCurrentDirectory = (PackageName . takeBaseName) <$> getCurrentDirectory

readCabalFile :: FilePath -> IO GenericPackageDescription
readCabalFile = readPackageDescription normal

toPackageName :: Dependency -> PackageName
toPackageName (Dependency name _) = name

lookupRepo :: PackageName -> Hackage -> Maybe GitHubRepo
lookupRepo pkg db = listToMaybe . catMaybes . map parseRepo $ repos
    where
        repos   = fromMaybe [] $ toRepos <$> M.lookup (unPackageName pkg) db
        toRepos = sourceRepos . packageDescription . snd . M.findMax

parseRepo :: SourceRepo -> Maybe GitHubRepo
parseRepo src = case (repoType src, repoLocation src) of
    (Just Git, Just loc) -> parseLocation loc
    _                    -> Nothing

-- TODO: Too naive parsing
parseLocation :: String -> Maybe GitHubRepo
parseLocation loc
    | isGitHub && length ps > 5 =
        Just $ GitHubRepo { owner = ps !! 4, repo = ps !! 5 }
    | otherwise     = Nothing
    where
        isGitHub = isInfixOf "github.com" loc
        ps       = splitOneOf "/." loc
