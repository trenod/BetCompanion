{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ImportQualifiedPost #-}

module OddsApi
  ( -- * Domain types (clean, flat — used by the UI)
    MatchOdds (..)
  , MatchScore (..)
    -- * API actions
  , fetchNbaOdds
  , fetchNbaScores
    -- * Saving JSON to disk
  , saveOddsToFile
  , saveScoresToFile
    -- * Loading JSON from disk
  , loadOddsFromFile
  , loadScoresFromFile
    -- * Load-from-disk-or-fetch helpers
  , loadOrFetchOdds
  , loadOrFetchScores
    -- * File paths used by the helpers above
  , oddsJsonFile
  , scoresJsonFile
  , parlayJsonFile
    -- * Parlay persistence types and functions
  , StoredParlayLeg (..)
  , StoredParlay (..)
  , loadParlaysFromFile
  , saveParlaysToFile
  , appendParlayToFile
  ) where

import Control.Exception      (SomeException, try)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BL
import Data.List              (find)
import Data.Maybe             (fromMaybe)
import Data.Proxy             (Proxy (..))
import Data.Text              (Text)
import Data.Text qualified as T
import GHC.Generics           (Generic)
import Network.HTTP.Client    (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import System.Directory       (doesFileExist)

--Data type for odds. We only keep the fields we care about.
data MatchOdds = MatchOdds
  { moCommence  :: String 
  , moHomeTeam  :: String
  , moAwayTeam  :: String
  , moHomePrice :: Maybe Double  -- Betsson decimal odds for home win
  , moAwayPrice :: Maybe Double  -- Betsson decimal odds for away win
  } deriving (Show, Generic)

instance ToJSON MatchOdds   -- used when saving to disk
instance FromJSON MatchOdds -- in case we want to load a saved file later

--Data type for scores. Similar to MatchOdds but with completed flag and optional scores instead of prices.
data MatchScore = MatchScore
  { msCommence  :: String
  , msHomeTeam  :: String
  , msAwayTeam  :: String
  , msCompleted :: Bool
  , msHomeScore :: Maybe Int
  , msAwayScore :: Maybe Int
  } deriving (Show, Generic)

instance ToJSON MatchScore
instance FromJSON MatchScore

--Raw data types matching the JSON structure returned by the API. 
--These are used for parsing the API response and are then projected into the cleaner MatchOdds and MatchScore 
--types used by the UI.
data RawOddsGame = RawOddsGame
  { rogCommenceTime :: Text
  , rogHomeTeam     :: Text
  , rogAwayTeam     :: Text
  , rogBookmakers   :: [RawBookmaker]
  } deriving (Show)

-- The FromJSON instances below use the aeson combinators to parse the JSON objects into our Haskell types.
instance FromJSON RawOddsGame where
  parseJSON = withObject "RawOddsGame" $ \o ->
    RawOddsGame
      <$> o .: "commence_time"
      <*> o .: "home_team"
      <*> o .: "away_team"
      <*> o .: "bookmakers"

data RawBookmaker = RawBookmaker
  { rbKey     :: Text   -- e.g. "betsson"
  , rbMarkets :: [RawMarket]
  } deriving (Show)

instance FromJSON RawBookmaker where
  parseJSON = withObject "RawBookmaker" $ \o ->
    RawBookmaker
      <$> o .: "key"
      <*> o .: "markets"

data RawMarket = RawMarket
  { rmKey      :: Text   -- e.g. "h2h"
  , rmOutcomes :: [RawOutcome]
  } deriving (Show)

instance FromJSON RawMarket where
  parseJSON = withObject "RawMarket" $ \o ->
    RawMarket
      <$> o .: "key"
      <*> o .: "outcomes"

data RawOutcome = RawOutcome
  { roName  :: Text
  , roPrice :: Double
  } deriving (Show)

instance FromJSON RawOutcome where
  parseJSON = withObject "RawOutcome" $ \o ->
    RawOutcome
      <$> o .: "name"
      <*> o .: "price"

data RawScoreGame = RawScoreGame
  { rsgCommenceTime :: Text
  , rsgHomeTeam     :: Text
  , rsgAwayTeam     :: Text
  , rsgCompleted    :: Bool
  , rsgScores       :: Maybe [RawScore]   -- null until the game starts
  } deriving (Show)

instance FromJSON RawScoreGame where
  parseJSON = withObject "RawScoreGame" $ \o ->
    RawScoreGame
      <$> o .: "commence_time"
      <*> o .: "home_team"
      <*> o .: "away_team"
      <*> o .: "completed"
      <*> o .:? "scores"

data RawScore = RawScore
  { rscoreName  :: Text
  , rscoreScore :: Text   -- API returns score as a string, e.g. "115"
  } deriving (Show)

instance FromJSON RawScore where
  parseJSON = withObject "RawScore" $ \o ->
    RawScore
      <$> o .: "name"
      <*> o .: "score"

--Project the raw API data into our cleaner domain types. This is where we extract the Betsson odds and convert 
--the score strings to Ints.
projectOdds :: RawOddsGame -> MatchOdds
projectOdds g = MatchOdds
  { moCommence  = T.unpack (rogCommenceTime g)
  , moHomeTeam  = T.unpack (rogHomeTeam g)
  , moAwayTeam  = T.unpack (rogAwayTeam g)
  , moHomePrice = priceFor (rogHomeTeam g)
  , moAwayPrice = priceFor (rogAwayTeam g)
  }
  where
    -- Find the Betsson bookmaker, then the h2h market, then look up the
    -- outcome whose name matches the team. Anything missing → Nothing.
    priceFor :: Text -> Maybe Double
    priceFor team = do
      bm     <- find (\b -> rbKey b == "betsson") (rogBookmakers g)
      market <- find (\m -> rmKey m == "h2h")     (rbMarkets bm)
      outc   <- find (\o -> roName o == team)    (rmOutcomes market)
      pure (roPrice outc)

--Same as above but for scores. We look up the score for each team by name, then parse the score string into an Int.
projectScore :: RawScoreGame -> MatchScore
projectScore g = MatchScore
  { msCommence  = T.unpack (rsgCommenceTime g)
  , msHomeTeam  = T.unpack (rsgHomeTeam g)
  , msAwayTeam  = T.unpack (rsgAwayTeam g)
  , msCompleted = rsgCompleted g
  , msHomeScore = scoreFor (rsgHomeTeam g)
  , msAwayScore = scoreFor (rsgAwayTeam g)
  }
  where
    scoreFor :: Text -> Maybe Int
    scoreFor team = do
      ss <- rsgScores g                                 -- Maybe [RawScore]
      s  <- find (\x -> rscoreName x == team) ss
      case reads (T.unpack (rscoreScore s)) of
        [(n, "")] -> Just n
        _         -> Nothing

--Servant API type and client functions. We define the API endpoints and query parameters according to the API docs, 
--then use 'client' to generate Haskell functions we can call.
type OddsAPI =
       "v4" :> "sports" :> "basketball_nba" :> "odds"
         :> QueryParam "regions"     String
         :> QueryParam "oddsFormat"  String
         :> QueryParam "apiKey"      String
         :> Get '[JSON] [RawOddsGame]
  :<|> "v4" :> "sports" :> "basketball_nba" :> "scores"
         :> QueryParam "daysFrom"    Int
         :> QueryParam "apiKey"      String
         :> Get '[JSON] [RawScoreGame]

-- The 'client' function generates two functions, 'getOdds' and 'getScores', that we can call to make the API requests. 
--They return a 'ClientM' action that we will run later.
oddsAPI :: Proxy OddsAPI
oddsAPI = Proxy

getOdds   :: Maybe String -> Maybe String -> Maybe String -> ClientM [RawOddsGame]
getScores :: Maybe Int    -> Maybe String                 -> ClientM [RawScoreGame]
getOdds :<|> getScores = client oddsAPI

--Fetch NBA odds from the API, then project the raw data into our cleaner MatchOdds type. We run the Servant ClientM
--action using the 'runOddsApi' helper defined below, which sets up the HTTP manager and base URL. We handle errors by 
--returning an 'Either String [MatchOdds]', where the Left case contains an error message and the Right case contains 
--the successful result.
fetchNbaOdds :: String -> IO (Either String [MatchOdds])
fetchNbaOdds key = do
  result <- runOddsApi $
    getOdds (Just "eu") (Just "decimal") (Just key)
  case result of
    Left err  -> return (Left (show err))
    Right raw -> return (Right (map projectOdds raw))

--Same as above but for scores. We pass the 'daysFrom' parameter to specify how many days back we want scores for.
fetchNbaScores :: String -> Int -> IO (Either String [MatchScore])
fetchNbaScores key daysFrom = do
  result <- runOddsApi $
    getScores (Just daysFrom) (Just key)
  case result of
    Left err  -> return (Left (show err))
    Right raw -> return (Right (map projectScore raw))

--Helper to run a Servant ClientM action. We create an HTTP manager with TLS settings, set up the base URL for the API,
--and then call 'runClientM' to execute the action. We return the result as an 'Either ClientError a' so the caller can 
--handle errors appropriately.
runOddsApi :: ClientM a -> IO (Either ClientError a)
runOddsApi action = do
  manager <- newManager tlsManagerSettings
  let env = mkClientEnv manager
              (BaseUrl Https "api.the-odds-api.com" 443 "")
  runClientM action env

--Saving to JSON files. We use the 'encodePretty' function from aeson-pretty to convert our Haskell values into 
--nicely formatted JSON, and then write it to a file. We handle any exceptions that might occur during file writing 
--and print an error message if it fails.

saveOddsToFile :: FilePath -> [MatchOdds] -> IO ()
saveOddsToFile = saveJson

saveScoresToFile :: FilePath -> [MatchScore] -> IO ()
saveScoresToFile = saveJson

saveJson :: ToJSON a => FilePath -> a -> IO ()
saveJson path value = do
  result <- try (BL.writeFile path (encodePretty value)) :: IO (Either SomeException ())
  case result of
    Right () -> return ()
    Left  e  -> putStrLn ("Could not write " ++ path ++ ": " ++ show e)

--Loading from JSON files. We read the file contents and then use 'eitherDecode' to parse the JSON into our 
--Haskell types.

-- File paths
oddsJsonFile, scoresJsonFile, parlayJsonFile :: FilePath
oddsJsonFile   = "nba_odds.json"
scoresJsonFile = "nba_scores.json"
parlayJsonFile = "nba_parlays.json"

loadOddsFromFile :: FilePath -> IO (Either String [MatchOdds])
loadOddsFromFile = loadJson

loadScoresFromFile :: FilePath -> IO (Either String [MatchScore])
loadScoresFromFile = loadJson

-- Persistence types for parlays saved by the user.
data StoredParlayLeg = StoredParlayLeg
  { splMatch :: MatchOdds
  , splSide  :: String  -- "Home" or "Away"
  } deriving (Show, Generic)

data StoredParlay = StoredParlay
  { spName :: String
  , spLegs :: [StoredParlayLeg]
  } deriving (Show, Generic)

instance ToJSON StoredParlayLeg
instance FromJSON StoredParlayLeg
instance ToJSON StoredParlay
instance FromJSON StoredParlay

-- Load and save parlays (the file contains an array of StoredParlay)
loadParlaysFromFile :: FilePath -> IO (Either String [StoredParlay])
loadParlaysFromFile = loadJson

saveParlaysToFile :: FilePath -> [StoredParlay] -> IO ()
saveParlaysToFile = saveJson

-- Append a single parlay to the parlay file (reads existing, appends, writes back).
appendParlayToFile :: FilePath -> StoredParlay -> IO ()
appendParlayToFile path par = do
  exists <- doesFileExist path
  if exists
    then do
      eres <- loadParlaysFromFile path
      case eres of
        Right xs -> saveParlaysToFile path (xs ++ [par])
        Left _   -> saveParlaysToFile path [par]
    else saveParlaysToFile path [par]

loadJson :: FromJSON a => FilePath -> IO (Either String a)
loadJson path = do
  result <- try (BL.readFile path) :: IO (Either SomeException BL.ByteString)
  case result of
    Left e     -> return (Left ("Could not read " ++ path ++ ": " ++ show e))
    Right bytes -> case eitherDecode bytes of
      Left err -> return (Left ("Could not parse " ++ path ++ ": " ++ err))
      Right v  -> return (Right v)

--Load-or-fetch helpers. These functions first check if the specified JSON file exists. If it does, they attempt to load
--the data from the file. If it doesn't exist, they fetch the data from the API, save it to the file, and then return 
--the fetched data. This way we can avoid unnecessary API calls and have a local cache of the data. 
--We return an 'Either String [MatchOdds]' or 'Either String [MatchScore]' to handle any errors that might occur during 
--loading or fetching.

loadOrFetchOdds :: String -> IO (Either String [MatchOdds])
loadOrFetchOdds key = do
  exists <- doesFileExist oddsJsonFile
  if exists
    then loadOddsFromFile oddsJsonFile
    else do
      r <- fetchNbaOdds key
      case r of
        Right xs -> saveOddsToFile oddsJsonFile xs >> return (Right xs)
        Left  e  -> return (Left e)

loadOrFetchScores :: String -> Int -> IO (Either String [MatchScore])
loadOrFetchScores key daysFrom = do
  exists <- doesFileExist scoresJsonFile
  if exists
    then loadScoresFromFile scoresJsonFile
    else do
      r <- fetchNbaScores key daysFrom
      case r of
        Right xs -> saveScoresToFile scoresJsonFile xs >> return (Right xs)
        Left  e  -> return (Left e)
