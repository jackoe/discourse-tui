{-# LANGUAGE OverloadedStrings #-}
module Main where
-- import System.Directory

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Network.HTTP.Simple
import Data.Aeson

main :: IO ()
main = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState

getTopicList :: IO TopicList
getTopicList = do
    request <-parseRequest "https://discourse.haskell.org/top.json" 
    resp <- httpJSON request
    return (getResponseBody resp :: TopicList)

data TuiState =
    TuiState TopicList
    deriving (Show)

type ResourceName = String

tuiApp :: App TuiState e ResourceName
tuiApp =
    App
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure
        , appAttrMap = const $ attrMap mempty []
        }

buildInitialState :: IO TuiState
buildInitialState = TuiState <$> getTopicList

drawTui :: TuiState -> [Widget ResourceName]
drawTui (TuiState (TopicList topics)) = (:[]) . viewport "posts" Vertical . vBox . map (str . title) $ topics

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e = halt s

instance FromJSON Topic where
    parseJSON = withObject "Topic" $ \v -> do 
        id <- v .: "id"
        title <- v .: "title"
        return $ Topic id title


instance FromJSON TopicList where
    parseJSON = withObject "TopicList" $ \v -> do
            topicListWrapper <- v .: "topic_list"
            topics <- topicListWrapper .: "topics"
            return $ TopicList topics

-- here to help with decoding from JSON
newtype TopicList = TopicList [Topic] deriving (Show)

data Topic = Topic {
                 postId :: Int,
                 title :: String
                 } deriving (Show)
