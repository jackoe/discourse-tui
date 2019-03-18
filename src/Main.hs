{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick
import Network.HTTP.Simple
import Data.Aeson
import qualified Cursor.List.NonEmpty as NE
import Cursor.Simple.List.NonEmpty (NonEmptyCursor)
import qualified Cursor.Simple.List.NonEmpty as SNE
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes
import Data.Maybe

main :: IO ()
main = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState

getCursor :: IO (NonEmptyCursor Topic)
getCursor = do
    request <- parseRequest "https://discourse.haskell.org/latest.json" 
    resp <- httpJSON request
    return . topicListToCursor . getResponseBody $ resp

newtype TuiState =
    TuiState {
              cursor :: NonEmptyCursor Topic
             }
    deriving (Show)

type ResourceName = String

tuiApp :: App TuiState e ResourceName
tuiApp =
    App
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure
        , appAttrMap = const $ attrMap mempty [("selected", fg red)]
        }

buildInitialState :: IO TuiState
buildInitialState = TuiState <$> getCursor

drawTui :: TuiState -> [Widget ResourceName]
drawTui (TuiState (NE.NonEmptyCursor prev curr next)) = (:[]) . viewport "posts" Vertical . vBox $
        (map drawPost . reverse $ prev)
    ++  (withAttr "selected" .  drawPost $ curr) :
        (map drawPost next)

drawPost topic = likes <+> title'
    where
        likes :: Widget ResourceName
        likes = padRight (Pad 1) . hLimit 4 . padRight Max $ (str . show . likeCount $ topic)

        title' :: Widget ResourceName
        title' = str . title $ topic



handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent (TuiState {cursor = cursor}) (VtyEvent (EvKey KUp   []))
        = continue . TuiState . fromMaybe cursor . SNE.nonEmptyCursorSelectPrev $ cursor
handleTuiEvent (TuiState {cursor = cursor}) (VtyEvent (EvKey KDown []))
        = continue . TuiState . fromMaybe cursor . SNE.nonEmptyCursorSelectNext $ cursor
handleTuiEvent s e = halt s

instance FromJSON Topic where
    parseJSON = withObject "Topic" $ \v -> do 
        postId' <- v .: "id"
        title'  <- v .: "title"
        likeCount' <- v.: "like_count"
        return $ Topic postId' title' likeCount'


instance FromJSON TopicList where
    parseJSON = withObject "TopicList" $ \v -> do
            topicListWrapper <- v .: "topic_list"
            topics <- topicListWrapper .: "topics"
            return $ TopicList topics

topicListToCursor :: TopicList -> NonEmptyCursor Topic
topicListToCursor (TopicList (x:xs)) = NE.NonEmptyCursor [] x xs

-- here to help with decoding from JSON
newtype TopicList = TopicList [Topic] deriving (Show)

data Topic = Topic {
                 postId :: Int,
                 title :: String,
                 likeCount :: Int
                 } deriving (Show)
