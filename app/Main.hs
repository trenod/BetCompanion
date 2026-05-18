{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Brick
import Brick.Widgets.List
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Monad.IO.Class (liftIO)
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Graphics.Vty
import Lens.Micro.TH
import Lens.Micro ((^.))
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import OddsApi
  ( fetchNbaOdds
  , fetchNbaScores
  , saveOddsToFile
  , saveScoresToFile
  , loadOrFetchOdds
  , loadOrFetchScores
  , oddsJsonFile
  , scoresJsonFile
  , MatchOdds (..)
  , MatchScore (..)
  , parlayJsonFile
  , StoredParlay(..)
  , StoredParlayLeg(..)
  , appendParlayToFile
  , loadParlaysFromFile
  )


data Screen
  = MainMenu
  | Odds
  | Scores
  | CurrentParlay
  | PastParlays
  | Statistics
  | PastParlayDetail  -- View for showing a single saved parlay's details
  deriving (Show, Eq)

data Name = MainList | SubList deriving (Show, Eq, Ord)

--What side of a match the user has bet on for a given parlay leg.
data BetSide = Home | Away deriving (Show, Eq)

--A single leg of a parlay bet: a match and which side the user has bet on.
data ParlayLeg = ParlayLeg
  { legMatch :: MatchOdds
  , legSide  :: BetSide
  } deriving (Show)

--The state of the app, including screen, menu lists, and loaded data.
data AppState = AppState
  { _screen        :: Screen
  , _previousScreen :: Maybe Screen -- To keep track of where we came from when viewing a saved parlay detail
  , _mainList      :: List Name String
  , _subList       :: List Name String
  , _oddsData      :: [MatchOdds]    -- upcoming matches with odds
  , _scoresData    :: [MatchScore]   -- recent matches with results
  , _currentParlay :: [ParlayLeg]    -- legs the user has added
  , _savedParlays  :: [StoredParlay] -- persisted parlays loaded from disk
  }

makeLenses ''AppState

--Dummy data for the Statistics screen, which is not implemented in this version.
statsItems :: Vector String
statsItems =
  Vec.fromList
    [ "Total bets placed : 0"
    , "Bets won          : 0"
    , "Bets lost         : 0"
    , "Win rate          : -- %"
    , "Total staked      : 0 NOK"
    , "Total returned    : 0 NOK"
    , "Net profit        : 0 NOK"
    ]


--Formatting a MatchOdds into a single line string for display in the Odds screen.
formatOdds :: MatchOdds -> String
formatOdds m =
  moCommence m
  ++ "  "
  ++ moHomeTeam m
  ++ " vs "
  ++ moAwayTeam m
  ++ "   | "
  ++ priceStr (moHomePrice m)
  ++ " / "
  ++ priceStr (moAwayPrice m)
  where
    priceStr Nothing  = "--"
    priceStr (Just p) = show p

--Format a list of MatchOdds into menu items for the Odds screen.
oddsToMenuItems :: [MatchOdds] -> Vector String
oddsToMenuItems [] = Vec.fromList ["(No upcoming matches loaded)"]
oddsToMenuItems xs = Vec.fromList (map formatOdds xs)

--Format the current parlay into menu items for display in the Current Parlay screen.
parlayToMenuItems :: [ParlayLeg] -> Vector String
parlayToMenuItems = Vec.fromList . map fmt
  where
    fmt (ParlayLeg m side) = formatOdds m ++ " | " ++ sideTag side
    sideTag Home = "H"
    sideTag Away = "A"

--Format a list of MatchScore into menu items for the Scores screen.
scoresToMenuItems :: [MatchScore] -> Vector String
scoresToMenuItems [] = Vec.fromList ["(No recent matches loaded)"]
scoresToMenuItems xs = Vec.fromList (map fmt xs)
  where
    fmt m =
      msCommence m
      ++ "  "
      ++ msHomeTeam m
      ++ " vs "
      ++ msAwayTeam m
      ++ "   | "
      ++ scoreStr m
    scoreStr m
      | not (msCompleted m) = "scheduled"
      | otherwise = case (msHomeScore m, msAwayScore m) of
          (Just h, Just a) -> show h ++ " - " ++ show a
          _                -> "completed (no score data)"

-- Convert saved parlays to menu items (show parlay name)
parlaysToMenuItems :: [StoredParlay] -> Vector String
parlaysToMenuItems [] = Vec.fromList ["(No past parlays saved)"]
parlaysToMenuItems xs = Vec.fromList (map spName xs)

-- Format a StoredParlayLeg for display
formatStoredLeg :: StoredParlayLeg -> String
formatStoredLeg (StoredParlayLeg m side) = formatOdds m ++ " | " ++ side

--The main menu items for the Main Menu screen.
mainMenuItems :: Vector String
mainMenuItems =
  Vec.fromList ["Odds", "Scores", "Current parlay", "Past parlays", "Statistics"]

--The initial state of the app when it starts up. Starts on the Main Menu with no data loaded and empty parlay.
initialState :: AppState
initialState = AppState
  { _screen        = MainMenu
  , _mainList      = list MainList mainMenuItems 1
  , _subList       = list SubList Vec.empty 1
  , _oddsData      = []
  , _scoresData    = []
  , _currentParlay = []
  , _savedParlays  = []
  , _previousScreen = Nothing
  }

--Draw the UI based on the current screen. The main menu and sub-screens have different layouts, 
--so we delegate to separate functions for each.
drawUI :: AppState -> [Widget Name]
drawUI st = [ui]
  where
    ui = case st ^. screen of
      MainMenu      -> drawMainMenu st
      Odds          -> drawSubScreen "Odds"           helpOdds   (st ^. subList)
      Scores        -> drawSubScreen "Scores"         helpRefr   (st ^. subList)
      CurrentParlay -> drawSubScreen "Current Parlay" helpPlain  (st ^. subList)
      PastParlays   -> drawSubScreen "Past Parlays"   helpPlain  (st ^. subList)
      PastParlayDetail -> drawSubScreen "Parlay"      helpPlain  (st ^. subList)
      Statistics    -> drawSubScreen "Statistics"     helpPlain  (st ^. subList)
    helpOdds  = "Press h/a to bet on home/away, r to refresh, Backspace or Esc to return."
    helpRefr  = "Press r to refresh, Backspace or Esc to return to the main menu."
    helpPlain = "Press Backspace or Esc to return to the main menu."

--Drawing the main menu: a centered box with instructions and a list of options. The selected option is highlighted.
drawMainMenu :: AppState -> Widget Name
drawMainMenu st =
  center $
  borderWithLabel (str " Bet Companion ") $
  hLimit 70 $
  vBox
    [ padBottom (Pad 1) $ str "Use arrow keys to navigate, Enter to select, q to quit."
    , renderList renderItem True (st ^. mainList)
    ]
  where
    renderItem selected item =
      let w = str item
      in if selected then withAttr selectedAttr w else w

--Drawing the sub-screens (Odds, Scores, Current Parlay, Past Parlays, Statistics): similar layout to the main menu 
--but with different instructions and a different list of items. The selected item is highlighted.
drawSubScreen :: String -> String -> List Name String -> Widget Name
drawSubScreen title helpText lst =
  center $
  borderWithLabel (str (" " ++ title ++ " ")) $
  hLimit 80 $
  vBox
    [ padBottom (Pad 1) $ str helpText
    , renderList renderItem True lst
    ]
  where
    renderItem selected item =
      let w = str item
      in if selected then withAttr selectedAttr w else w

--Handling events: key presses and other interactions. The behavior depends on which screen we're on, 
--so we delegate to separate functions for the main menu and sub-screens.
handleEvent :: BrickEvent Name () -> EventM Name AppState ()
handleEvent ev = do
  st <- get
  case st ^. screen of
    MainMenu -> handleMainMenu ev
    _        -> handleSubScreen ev

--Handling events on the main menu: Enter to select an option, q to quit, arrow keys to navigate the list.
handleMainMenu :: BrickEvent Name () -> EventM Name AppState ()
handleMainMenu (VtyEvent (EvKey KEnter [])) = do
  st <- get
  case listSelectedElement (st ^. mainList) of
    Nothing        -> return ()
    Just (_, item) -> navigateTo item
handleMainMenu (VtyEvent (EvKey (KChar 'q') [])) = halt
handleMainMenu (VtyEvent ev) = zoom mainList (handleListEvent ev)
handleMainMenu _ = return ()

--Navigate to a sub-screen based on the selected main menu item. For Odds and Scores, we need to load the data first 
--(from disk or API) before showing the sub-screen. For the other options, we can directly show the sub-screen with 
--precomputed items.
navigateTo :: String -> EventM Name AppState ()
navigateTo "Odds"           = loadOddsScreen
navigateTo "Scores"         = loadScoresScreen
navigateTo "Current parlay" = do
  st <- get
  showSubScreen CurrentParlay (parlayToMenuItems (st ^. currentParlay))
navigateTo "Past parlays"   = do
  -- load saved parlays from disk and show list of names
  result <- liftIO (loadParlaysFromFile parlayJsonFile)
  case result of
    Right xs -> do
      modify $ \s -> s { _savedParlays = xs }
      showSubScreen PastParlays (parlaysToMenuItems xs)
    Left _ -> showSubScreen PastParlays (Vec.fromList ["(Could not load past parlays)"])
navigateTo "Statistics"     = showSubScreen Statistics statsItems
navigateTo _                = return ()

--Helper function to switch to a sub-screen and set the list of items to display. Used by 'navigateTo' 
--after loading  data.
showSubScreen :: Screen -> Vector String -> EventM Name AppState ()
showSubScreen newScreen items = modify $ \s -> s
  { _screen  = newScreen
  , _subList = list SubList items 1
  }

--Load the odds data (from disk or API) and enter the Odds screen. If loading fails, show an error message instead.
loadOddsScreen :: EventM Name AppState ()
loadOddsScreen = do
  result <- liftIO (loadOrFetchOdds apiKey)
  case result of
    Right xs -> do
      modify $ \s -> s { _oddsData = xs }
      showSubScreen Odds (oddsToMenuItems xs)
    Left err ->
      showSubScreen Odds (Vec.fromList ["Error loading odds:", err])

--Load the scores data (from disk or API) and enter the Scores screen. If loading fails, show an error message instead.
loadScoresScreen :: EventM Name AppState ()
loadScoresScreen = do
  result <- liftIO (loadOrFetchScores apiKey 3)
  case result of
    Right xs -> do
      modify $ \s -> s { _scoresData = xs }
      showSubScreen Scores (scoresToMenuItems xs)
    Left err ->
      showSubScreen Scores (Vec.fromList ["Error loading scores:", err])

--Refresh the odds data by fetching from the API again. Update the state with the new data and save it to disk. 
--If refreshing fails, show an error message.
refreshOdds :: EventM Name AppState ()
refreshOdds = do
  result <- liftIO (fetchNbaOdds apiKey)
  case result of
    Right xs -> do
      liftIO (saveOddsToFile oddsJsonFile xs)
      modify $ \s -> s
        { _oddsData = xs
        , _subList  = list SubList (oddsToMenuItems xs) 1
        }
    Left err ->
      modify $ \s -> s
        { _subList = list SubList
            (Vec.fromList ["Refresh failed:", err]) 1
        }

--Refresh the scores data by fetching from the API again. Update the state with the new data and save it to disk. 
--If refreshing fails, show an error message.
refreshScores :: EventM Name AppState ()
refreshScores = do
  result <- liftIO (fetchNbaScores apiKey 3)
  case result of
    Right xs -> do
      liftIO (saveScoresToFile scoresJsonFile xs)
      modify $ \s -> s
        { _scoresData = xs
        , _subList    = list SubList (scoresToMenuItems xs) 1
        }
    Left err ->
      modify $ \s -> s
        { _subList = list SubList
            (Vec.fromList ["Refresh failed:", err]) 1
        }

handleSubScreen :: BrickEvent Name () -> EventM Name AppState ()
handleSubScreen ev = case ev of
  VtyEvent (EvKey KEsc []) -> goBack
  VtyEvent (EvKey KBS  []) -> goBack
  VtyEvent (EvKey (KChar 'q') []) -> halt
  VtyEvent (EvKey (KChar 'r') []) -> do
    st <- get
    case st ^. screen of
      Odds   -> refreshOdds
      Scores -> refreshScores
      _      -> return ()
  VtyEvent (EvKey (KChar 'h') []) -> do
    st <- get
    case st ^. screen of
      Odds -> placeBet Home
      _    -> return ()
  VtyEvent (EvKey (KChar 'a') []) -> do
    st <- get
    case st ^. screen of
      Odds -> placeBet Away
      _    -> return ()
  VtyEvent (EvKey (KChar 's') []) -> do
    st <- get
    case st ^. screen of
      CurrentParlay -> saveCurrentParlay
      _             -> return ()
  VtyEvent (EvKey KEnter []) -> do
    st <- get
    case st ^. screen of
      PastParlays -> case listSelectedElement (st ^. subList) of
        Nothing -> return ()
        Just (i, _) -> case safeIndex i (st ^. savedParlays) of
          Nothing -> return ()
          Just sp -> showSubScreen PastParlayDetail (Vec.fromList (map formatStoredLeg (spLegs sp)))
      _ -> return ()
  VtyEvent ev' -> zoom subList (handleListEvent ev')
  _ -> return ()

--Make a bet on the selected match in the Odds screen by adding a new leg to the current parlay. 
--The leg includes the match and which side (home/away) the user has bet on. If no match is selected, do nothing.
placeBet :: BetSide -> EventM Name AppState ()
placeBet side = do
  st <- get
  case listSelected (st ^. subList) of
    Nothing -> return ()
    Just i  ->
      case safeIndex i (st ^. oddsData) of
        Nothing -> return ()
        Just m  ->
          let leg = ParlayLeg m side
          in modify $ \s -> s { _currentParlay = (s ^. currentParlay) ++ [leg] }

--A safe version of list indexing that returns Nothing if the index is out of bounds, instead of throwing an error.
safeIndex :: Int -> [a] -> Maybe a
safeIndex i xs
  | i < 0          = Nothing
  | i >= length xs = Nothing
  | otherwise      = Just (xs !! i)

--Go back to the main menu by setting the screen to MainMenu. The sub-list will be replaced when we navigate to a 
--new sub-screen, so we don't need to clear it here.
goBack :: EventM Name AppState ()
goBack = modify $ \st -> st { _screen = MainMenu }


-- Save the current parlay to disk with a timestamped name.
saveCurrentParlay :: EventM Name AppState ()
saveCurrentParlay = do
  st <- get
  let legs = st ^. currentParlay
  if null legs
    then return ()
    else do
      now <- liftIO getCurrentTime
      let name = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
          storedLegs = map toStored legs
          par = StoredParlay ("Parlay " ++ name) storedLegs
      liftIO $ appendParlayToFile parlayJsonFile par
      return ()
  where
    toStored (ParlayLeg m side) = StoredParlayLeg m (case side of Home -> "Home"; Away -> "Away")

--Attributes and styling: define a custom attribute for the selected item in the lists, which will have a 
--blue background and white foreground.
selectedAttr :: AttrName
selectedAttr = attrName "selected"

theAttrMap :: AttrMap
theAttrMap = attrMap defAttr
  [ (selectedAttr, withBackColor (withForeColor defAttr white) blue)
  ]


-- The API key for the-odds-api.com
apiKey :: String
apiKey = "807b3198e31e0ec85f3bd2541a156d0e"

-- The main entry point of the application. We create the Brick app with the appropriate drawing and event handling 
--functions,
main :: IO ()
main = do
  let app = App
        { appDraw         = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent  = handleEvent
        , appStartEvent   = return ()
        , appAttrMap      = const theAttrMap
        }
  _ <- defaultMain app initialState
  return ()
