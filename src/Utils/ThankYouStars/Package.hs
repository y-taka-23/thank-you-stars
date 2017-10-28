module Utils.ThankYouStars.Package (
      dependentRepos
    , getCabalFiles
    , readCabalFile
    , readStackIndex
    ) where

import Utils.ThankYouStars.GitHub

import           Data.List                             ( isInfixOf, isPrefixOf )
import           Data.List.Split                       ( splitOneOf )
import qualified Data.Map                              as M
import qualified Data.Set                              as S
import           Data.Maybe
import           Distribution.Hackage.DB               ( Hackage, readHackage' )
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse ( readPackageDescription )
import           Distribution.Verbosity                ( normal )
import           System.Directory                      ( getCurrentDirectory
                                                       , getAppUserDataDirectory
                                                       , getPermissions
                                                       , searchable
                                                       , listDirectory
                                                       )
import           System.FilePath                       ( joinPath
                                                       , combine
                                                       , takeExtension
                                                       )

allBuildDepends :: GenericPackageDescription -> S.Set PackageName
allBuildDepends desc =
    let ls = map (libBuildInfo . condTreeData) . maybeToList . condLibrary   $ desc
        es = map (buildInfo          . condTreeData . snd) . condExecutables $ desc
        ts = map (testBuildInfo      . condTreeData . snd) . condTestSuites  $ desc
        bs = map (benchmarkBuildInfo . condTreeData . snd) . condBenchmarks  $ desc
    in  S.fromList . concatMap depends $ ls ++ es ++ ts ++ bs

depends :: BuildInfo -> [PackageName]
depends = map toPackageName . targetBuildDepends

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
readCabalFile = readPackageDescription normal

dependentRepos :: Hackage -> GenericPackageDescription -> S.Set GitHubRepo
dependentRepos db desc = S.map fromJust $ S.filter isJust mRepos
    where
        pkgs    = S.delete (packageName desc) (allBuildDepends desc)
        mRepos  = S.map (flip lookupRepo $ db) pkgs

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

readStackIndex :: IO Hackage
readStackIndex = getStackIndexPath >>= readHackage'

getStackIndexPath :: IO FilePath
getStackIndexPath = do
    stackDir <- getAppUserDataDirectory "stack"
    return $ joinPath [ stackDir, "indices", "Hackage", "00-index.tar" ]
