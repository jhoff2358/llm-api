{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ApiConfig where

import GHC.Generics
import Data.Aeson
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as B
import Data.Yaml

data Config = Config
   { api :: ApiConfig
   , request :: RequestConfig
   } deriving (Show, Generic)

data ApiConfig = ApiConfig
   { baseUrl :: S8.ByteString
   , endpoint :: S8.ByteString
   , apiKey :: S8.ByteString
   , anthropicVersion :: S8.ByteString
   , timeout :: Int
   } deriving (Show, Generic)

data RequestConfig = RequestConfig
   { model :: String
   , max_tokens :: Int
   , temperature :: Double
   , system :: String
   } deriving (Show, Generic)

instance FromJSON Config
instance FromJSON RequestConfig
instance FromJSON ApiConfig where
  parseJSON = withObject "ApiConfig" $ \v -> do
    baseUrl' <- S8.pack <$> v .: "baseUrl"
    endpoint' <- S8.pack <$> v .: "endpoint"
    anthropicVersion' <- S8.pack <$> v .: "anthropicVersion"
    apiKey' <- S8.pack <$> v .: "apiKey"
    timeout' <- v .: "timeout"
    return $ ApiConfig baseUrl' endpoint' apiKey' anthropicVersion' timeout'

readConfig :: String -> IO Config
readConfig fileName = do
   configFile <- B.readFile fileName
   return $ either (error . show) id (decodeEither' (B.toStrict configFile) :: Either ParseException Config)

--buildRequest :: Config -> String -> Request