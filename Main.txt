{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import GHC.Generics
import qualified ApiConfig as AC
import Data.Yaml
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as B

data RequestBody = RequestBody
   { model :: String
   , max_tokens :: Int
   , temperature :: Double
   , system :: String
   , messages :: [Message]
   } deriving (Show, Generic)

data Message = Message
   { role :: String
   , content :: [Content]
   } deriving (Show, Generic)

data Content = Content
   { contentType :: String
   , text :: String
   } deriving (Show, Generic)

instance ToJSON RequestBody
instance ToJSON Message
instance ToJSON Content where
   toJSON Content{..} = object
      [ "type" .= contentType
      , "text" .= text
      ]

   toEncoding Content{..} = pairs
      ( "type" .= contentType
      <> "text" .= text
      )

---- Incomplete, just grabs the content field. TODO fill out remaining fields
--data ApiResponse = ApiResponse
--   { content :: [ResposeContent]
--   } deriving (Show, Generic)
--
--data ResponseContent = ResponseContent
--   { text' :: Text
--   } deriving (Show, Generic)
--
--instance FromJSON ApiResponse
--instance FromJSON Content
--
--processResponse :: ApiResponse -> IO ()
--processResponse response = do
--   let textContent = concatMap extractText (content response)
--   putStrLn $ T.unpack $ T.decodeUtf8 $ T.encudeUtf8 textContent

textContent :: String -> Content
textContent str = Content { contentType = "text", text = str }

userMessage :: String -> Message
userMessage str = Message { role = "user", content = [textContent str] }

buildSimpleBody :: AC.RequestConfig -> String -> RequestBody
buildSimpleBody (AC.RequestConfig model' max_tokens' temperature' system') str = 
   RequestBody
   { model = model'
   , max_tokens = max_tokens'
   , temperature = temperature'
   , system = system'
   , messages = [userMessage str]
   }

main :: IO ()
main = do
  -- Read the configuration file
  config <- AC.readConfig "config.yaml"

  -- Prepare the API request
  let api' = AC.api config
      headers = [("x-api-key", AC.apiKey api'),
                 ("Content-Type", "application/json"),
                 ("anthropic-version", AC.anthropicVersion api')]
      body = buildSimpleBody (AC.request config) "What are 3 ways to cook apples?"
      request' = setRequestMethod "POST"
              $ setRequestSecure True
              $ setRequestPort 443
              $ setRequestPath (AC.endpoint api')
              $ setRequestHeaders headers
              $ setRequestBodyJSON body
              $ setRequestHost (AC.baseUrl api') defaultRequest

  -- Send the API request
  response <- httpLBS request'

  -- Process the API response
  putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
  putStrLn $ "Response body: " ++ show (getResponseBody response)