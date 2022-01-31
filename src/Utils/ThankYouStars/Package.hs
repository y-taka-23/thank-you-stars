module Utils.ThankYouStars.Package (
      dependentRepos
    , getCabalFiles
    , readCabalFile
    , readHackageDB
    ) where

import Utils.ThankYouStars.GitHub

import           Data.List
    ( isInfixOf
    , isPrefixOf
    )
import           Data.List.Split                               ( splitOneOf )
import qualified Data.Map                                      as M
import           Data.Maybe
import qualified Data.Set                                      as S
import           Distribution.Hackage.DB
    ( HackageDB
    , cabalFile
    , hackageTarball
    , readTarball
    )
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Configuration
    ( flattenPackageDescription
    )
import           Distribution.PackageDescription.Parsec
    ( readGenericPackageDescription
    )
import           Distribution.Verbosity                        ( normal )
import           System.Directory
    ( getCurrentDirectory
    , getPermissions
    , listDirectory
    , searchable
    )
import           System.FilePath
    ( combine
    , takeExtension
    )

getCabalFiles :: IO (S.Set FilePath)
getCabalFiles = getCurrentDirectory >>= searchCabalFiles

searchCabalFiles :: FilePath -> IO (S.Set FilePath)
searchCabalFiles fp = do
    p <- getPermissions fp
    if searchable p
        then do
            children <- map (combine fp) . filter visible <$> listDirectory fp
            S.unions <$> mapM searchCabalFiles children
        else do
            if takeExtension fp == ".cabal"
                then return $ S.singleton fp
                else return $ S.empty

visible :: FilePath -> Bool
visible fp = not $ "." `isPrefixOf` fp

readCabalFile :: FilePath -> IO GenericPackageDescription
readCabalFile = readGenericPackageDescription normal

dependentRepos :: HackageDB -> GenericPackageDescription -> S.Set GitHubRepo
dependentRepos db desc = S.map fromJust $ S.filter isJust mRepos
    where
        excepts = [mkPackageName "base", packageName desc]
        pkgs    = foldr S.delete (allDependencies desc) excepts
        mRepos  = S.map (flip lookupRepo $ db) pkgs

allDependencies :: GenericPackageDescription -> S.Set PackageName
allDependencies = S.fromList . map toPackageName . allBuildDepends . flattenPackageDescription
    where
        toPackageName (Dependency name _ _) = name

lookupRepo :: PackageName -> HackageDB -> Maybe GitHubRepo
lookupRepo pkg db = listToMaybe . catMaybes . map parseRepo $ repos
    where
        repos   = fromMaybe [] $ toRepos <$> M.lookup pkg db
        toRepos = sourceRepos . flattenPackageDescription . cabalFile . snd . M.findMax

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

readHackageDB :: IO HackageDB
readHackageDB = hackageTarball >>= readTarball Nothing
