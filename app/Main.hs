{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Widgets.List
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Graphics.Vty
import Lens.Micro.TH
--import Data.Monoid
import Control.Monad
import Control.Monad.State qualified as State
--import qualified Control.Monad.State as MState
import Control.Monad.IO.Class (liftIO)
--import Data.Text (Text)
--import Data.Text qualified as T
--import Network.HTTP.Simple
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
--import Control.Monad.Trans.Either
--import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
--import Servant.API
--import Servant.Client

data State = State {_listyPart :: GenericList Int Seq (Widget Int),
                    _matchIDcounter :: Integer,
                    _betIDcounter :: Integer,
                    _parlays :: [Parlay],
                    _oddsData :: [Either OddsSummaryBasketball OddsSummarySoccer],
                    _resultsData :: [Either ResultSummaryBasketball ResultSummarySoccer]
                    }

makeLenses ''State

type ID = Integer -- The ID of an entity, primary key
type SportsID = (Sport, ID) -- (sport type, match ID)
data Sport = Basketball | Soccer deriving (Show, Eq)
type BetID = (BetType, ID) -- (bet type, bet ID)
data BetType = Single | Parlay deriving (Show, Eq)

data Match = BasketballMatchData BasketballMatch | SoccerMatchData SoccerMatch deriving (Show)

data BasketballMatch = BasketballMatch
  { basketballMatchId :: SportsID -- The ID of the match, primary key
  , basketballMatchOdds :: (Double, Double) -- (home win, away win)
  , basketballMatchOutcome :: Maybe Integer -- 1 for home win, 2 for away win, Nothing for pending
  , basketballTeams :: (Text, Text) -- (home team, away team)
  } deriving (Show)

data SoccerMatch = SoccerMatch
  { soccerMatchId :: SportsID -- The ID of the match, primary key
  , soccerMatchOdds :: (Double, Double, Double) -- (home win, draw, away win)
  , soccerMatchOutcome :: Maybe Integer -- 1 for home win, 2 for draw, 3 for away win, Nothing for pending
  , soccerTeams :: (Text, Text) -- (home team, away team)
  } deriving (Show)

data BetAPIResult = BetAPIResult
  { resultBetId :: BetID -- The ID of the bet, primary key
  , resultMatches :: [Match] -- The matches included in the bet
  , resultOutcome :: Maybe Bool -- Nothing for pending, Just True for win, Just False for loss
  } deriving (Show)

data Parlay = Parlay
  { betID :: BetID -- The ID of the bet, primary key
  , betType :: BetType -- Single or Parlay
  , betMatches :: [Match] -- The matches included in the bet
  , betAmount :: Integer -- The amount of money wagered on the bet
  , combinedOdds :: Double -- The combined odds for the bet, calculated from the individual match odds
  , betOutcome :: Maybe Bool -- Nothing for pending, Just True for win, Just False for loss
  , betRisk :: Double -- A value between 0 and 1 representing the risk level of the bet
  } deriving (Show)
instance SQLRow Parlay

parlays :: Table Parlay
parlays = table "parlays" [#betID :- primary]

type OddsAPIBasketball = 
  "odds" :> Get '[JSON] [OddsSummaryBasketball]
  :<|> "results" :> Get '[JSON] [ResultSummaryBasketball]

type OddsAPISoccer =
  "odds" :> Get '[JSON] [OddsSummarySoccer]
  :<|> "results" :> Get '[JSON] [ResultSummarySoccer]

-- The OddsSummary types represent the data we get from the API for each match, which includes the match ID, sport type, teams, and odds value. The ResultSummary types represent the data we get from the API for the results of each match, which includes the match ID, sport type, teams, and outcome.
data OddsSummaryBasketball = OddsSummaryBasketball
  { basketballoddsId :: ID
  , oddsSport :: Sport
  , oddsTeams :: (Text, Text)
  , oddsValue :: Double
  } deriving (Generic, Show)

data OddsSummarySoccer = OddsSummarySoccer
  { oddsId :: ID
  , oddsSport :: Sport
  , oddsTeams :: (Text, Text)
  , oddsValue :: Double
  } deriving (Show)

-- The ResultSummary types represent the data we get from the API for the results of each match, which includes the match ID, sport type, teams, and outcome.
data ResultSummaryBasketball = ResultSummaryBasketball
  { resultId :: ID
  , resultSport :: Sport
  , resultTeams :: (Text, Text)
  , resultOutcome :: Maybe Integer
  } deriving (Show)

data ResultSummarySoccer = ResultSummarySoccer
  { resultId :: ID
  , resultSport :: Sport
  , resultTeams :: (Text, Text)
  , resultOutcome :: Maybe Integer
  } deriving (Show)

-- The FromJSON instances allow us to parse the JSON data we get from the API into our Haskell data types. We use the Aeson library to do this, and we define how to parse each field from the JSON object.
instance FromJSON OddsSummaryBasketball where
  parseJSON (Object o) =
    OddsSummaryBasketball <$> o .: "id"
                          <*> o .: "sport"
                          <*> o .: "teams"
                          <*> o .: "value"
  parseJSON _ = mzero
  
instance FromJSON OddsSummarySoccer where
  parseJSON (Object o) =
    OddsSummarySoccer <$> o .: "id"
                      <*> o .: "sport"
                      <*> o .: "teams"
                      <*> o .: "value"
  parseJSON _ = mzero

instance FromJSON ResultSummaryBasketball where
  parseJSON (Object o) =
    ResultSummaryBasketball <$> o .: "id"
                            <*> o .: "sport"
                            <*> o .: "teams"
                            <*> o .: "outcome"
  parseJSON _ = mzero 

instance FromJSON ResultSummarySoccer where
    parseJSON (Object o) =
      ResultSummarySoccer <$> o .: "id"
                          <*> o .: "sport"
                          <*> o .: "teams"
                          <*> o .: "outcome"    
    parseJSON _ = mzero




readExternalBets = undefined

getResultsFromAPI = undefined

getMockResultsForTesting = undefined

recordBets = undefined

calculateCombinedOdds = do
    combinedOdds <- oddsFromMatches matches ==> 

    combinedOdds <- foldM (\acc match -> do
        odds <- getOddsForMatch match
        return (acc * odds)) 1.0 matches

getOddsFromAPI = undefined

getMockOddsForTesting :: IO [Either OddsSummaryBasketball OddsSummarySoccer]
getMockOddsForTesting = do
    let mockOdds = [OddsSummaryBasketball 1 Basketball ("Team A", "Team B") 1.5, OddsSummarySoccer 2 Soccer ("Team C", "Team D") 2.0]
    return mockOdds

evaluateRisk :: Double -> IO Double
evaluateRisk odds = do
    -- Define thresholds for risk levels
    let lowRiskThreshold = 1.5
    let mediumRiskThreshold = 3.0

    -- Determine risk level based on odds
    if odds < lowRiskThreshold
        then return 0.2 -- Low risk
        else if odds < mediumRiskThreshold
            then return 0.5 -- Medium risk
            else return 0.8 -- High risk

generateAdvice :: Int -> Double -> IO String
generateAdvice numberofmatches riskLevel = do
    let advice = case riskLevel of
            0.2 -> "This bet is low risk. It has a good chance of winning, but the payout will be smaller."
            0.5 -> "This bet is medium risk. It has a moderate chance of winning, and the payout will be decent."
            0.8 -> "This bet is high risk. It has a lower chance of winning, but the payout will be higher."
    return advice

createHTMLReport :: [Parlay] -> IO ()
createHTMLReport parlays = do
    let htmlHeader = "<html><head><title>Betting Report</title></head><body><h1>Betting Report</h1><table border='1'><tr><th>Bet ID</th><th>Bet Type</th><th>Matches</th><th>Amount</th><th>Combined Odds</th><th>Outcome</th><th>Risk Level</th><th>Advice</th></tr>"
    let htmlFooter = "</table></body></html>"
    let htmlRows = concatMap parlayToHTMLRow parlays
    let htmlContent = htmlHeader ++ htmlRows ++ htmlFooter
    writeFile "betting_report.html" htmlContent 

insertParlayIntoDB :: Parlay -> IO ()
insertParlayIntoDB parlay = do
    with SQLite "parlays.sqlite" $ do
        createTable parlays
        insert_ parlays parlay


draw :: State -> Widget Int
draw (State l) = renderList showItem True l
  where
    showItem :: Bool -> Widget Int -> Widget Int
    showItem selected w
      | selected = withAttr (attrName "selected") w
      | otherwise = w

state0 :: State
state0 = State $ list 0 (Seq.fromList [str "hello", str "goodbye"]) 1

main :: IO ()
main = do
  with SQLite "parlays.sqlite" $ do
    -- Create the database and tables if they don't exist
    createTable parlays
    -- Insert some mock data for testing
    insert_ parlays (Parlay (Single, 1) Single [BasketballMatchData (BasketballMatch (Basketball, 1) (1.5, 2.5) Nothing ("Team A", "Team B"))] 100 1.5 Nothing 0.5)
    insert_ parlays (Parlay (Single, 2) Single [SoccerMatchData (SoccerMatch (Soccer, 2) (2.0, 3.0, 4.0) Nothing ("Team C", "Team D"))] 50 2.0 Nothing 0.7)   
  let app =
        App
          { appDraw = \state -> [draw state],
            appChooseCursor = showFirstCursor,
            appHandleEvent = handleEvent,
            appStartEvent = return (),
            appAttrMap = const (attrMap defAttr [(attrName "selected", bg blue)])
          }
  _ <- defaultMain app state0
  return ()

handleEvent :: BrickEvent Int () -> EventM Int State ()
handleEvent (VtyEvent ev) = zoom listyPart $ handleListEvent ev
handleEvent _ = return ()