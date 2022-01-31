{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Utils.ThankYouStars.GitHub (
      Token(..)
    , GitHubRepo(..)
    , readToken
    , starRepo
    ) where

import           Control.Exception     ( catch, throwIO )
import           Data.Aeson
import           Data.ByteString       ( ByteString )
import qualified Data.ByteString.Lazy  as BSL
import           Data.String           ( fromString )
import           Data.Text.Encoding    ( encodeUtf8 )
import           Data.Version          ( showVersion )
import           Network.HTTP.Req
import           Paths_thank_you_stars ( version )

data Token = Token {
      unToken :: ByteString
    } deriving ( Eq, Show )

instance FromJSON Token where
    parseJSON (Object v) = Token . encodeUtf8 <$> v .: "token"
    parseJSON _          = error "invalid format"

readToken :: FilePath -> IO (Either String Token)
readToken fp = eitherDecode <$> BSL.readFile fp

data GitHubRepo = GitHubRepo {
      owner :: String
    , repo  :: String
    } deriving ( Eq, Ord )

instance Show GitHubRepo where
    show ghr = "https://github.com/" ++ owner ghr ++ "/" ++ repo ghr

starringUrl :: GitHubRepo -> Url 'Https
starringUrl ghr =
    https "api.github.com" /: "user" /: "starred" /~ owner ghr /~ repo ghr

userAgent :: Option scheme
userAgent = header "User-Agent" agent
    where
        agent = "thank-you-stars/" <> fromString (showVersion version)

-- Warning suppressed by a GHC option
instance MonadHttp IO where
    handleHttpException = throwIO

starRepo :: Token -> GitHubRepo -> IO (Either HttpException ())
starRepo token ghr =
    (do
            let headers = oAuth2Token (unToken token) <> userAgent
            _ <- req PUT (starringUrl ghr) NoReqBody ignoreResponse headers
            return $ Right ()
        ) `catch` (\(e :: HttpException) -> do
            return $ Left e
        )
