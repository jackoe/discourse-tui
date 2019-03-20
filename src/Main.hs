{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Brick
import Network.HTTP.Simple
import Data.Aeson
import qualified Cursor.List.NonEmpty as NE
import Cursor.Simple.List.NonEmpty (NonEmptyCursor)
import qualified Cursor.Simple.List.NonEmpty as SNE
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes
import Data.Maybe
import Brick.Widgets.Border
import qualified Data.IntMap.Strict as M
import Brick.Widgets.List
import qualified Data.Vector as V
import Types

main :: IO ()
main = do
    initialState <- getTuiState
    endState <- defaultMain tuiApp initialState
    putStrLn "close"

getTuiState :: IO TuiState
getTuiState = do
    topicsRequest <- parseRequest "https://discourse.haskell.org/latest.json"
    categoriesRequest <- parseRequest "https://discourse.haskell.org/categories.json"
    topicsResp <- getResponseBody <$> httpJSON topicsRequest
    categoriesResp <- getResponseBody <$> httpJSON categoriesRequest
    return TuiState {
                    posts = Nothing,
                    topics = Just $ list "contents" (V.fromList . topicList $ topicsResp) topicHeight,
                    userMap = M.fromList . map (\x -> (userId x, x)) . users $ topicsResp,
                    categoryMap = M.fromList . map (\x -> (categoryId x, x)) . categories $ categoriesResp,
                    baseURL = "https://discourse.haskell.org/"
                    }


getPosts :: TuiState -> IO (List String Post)
getPosts (TuiState {baseURL = baseURL, topics = (Just topics)}) = do
    let (Just selectedTopicID) = topicId . snd <$> listSelectedElement topics 
    postsRequest <- parseRequest $ baseURL ++ "t/" ++ (show selectedTopicID) ++ ".json"
    (PostResponse l) <- getResponseBody <$> httpJSON postsRequest
    return $ list "posts" (V.fromList l) 5




type ResourceName = String

tuiApp :: App TuiState e ResourceName
tuiApp =
    App
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure
        , appAttrMap = const $ attrMap mempty [("title", withStyle currentAttr bold), ("pinned", fg green), ("selected", fg red), ("OP", fg blue), ("rest", defAttr)]
        }


drawTui :: TuiState -> [Widget ResourceName]
drawTui (TuiState (Just scrollable) Nothing userMap categoryMap _) = (:[]) . renderList drawTopic True $ scrollable
    where
        drawTopic selected (Topic _ categoryId title likeCount postsCount posters pinned)= 
                          (if selected then  withAttr "selected" . border else border)
                        . (if pinned   then  withAttr "pinned"   else id)
                        . padRight Max
                        $ (likes <+> title') <=> hBox [category, postsCount', posters']
            where
                likes :: Widget ResourceName
                likes = padRight (Pad 1)
                      . hLimit 4
                      . padRight Max
                      . str
                      . show
                      $ likeCount

                title' :: Widget ResourceName
                title' = withAttr "title" . str $ title

                postsCount' :: Widget ResourceName
                postsCount' = padLeft (Pad 5)
                            . str
                            . ("posts: " ++)
                            . show
                            $ postsCount

                posters' :: Widget ResourceName
                posters' = padLeft (Pad 5)
                       . hBox
                       . mapFst (withAttr "OP") (withAttr "rest")
                       . showList
                       . map (\x ->  userName $ userMap M.! posterId x)
                       $ posters

                category :: Widget ResourceName
                category = padLeft (Pad 5) . str . categoryName $ categoryMap M.! categoryId

                showList :: [String] -> [Widget ResourceName]
                showList s = map str $ (map (++ " ") . init $ s) ++ [last s]
drawTui (TuiState _ (Just posts) _ _ _) = (:[]) . renderList drawPost True $ posts
    where
        drawPost _ (Post id username contents) = border $ (withAttr "OP" . str $ username) <=> (strWrap $ contents)

mapFst :: (a -> a) -> (a -> a) ->  [a] -> [a]
mapFst fn fn' (x:xs) = (fn x) : (map fn' xs)

handleTuiEvent :: TuiState -> BrickEvent String e -> EventM String (Next TuiState)
handleTuiEvent tui (VtyEvent (EvKey (KChar 'q') _)) = halt tui
handleTuiEvent tui (VtyEvent (EvKey KRight  _)) = do
    posts' <- liftIO $ getPosts tui
    continue $ tui {posts = Just posts'}

handleTuiEvent tui (VtyEvent (EvKey KLeft   _)) = continue $ tui {posts = Nothing}
handleTuiEvent (TuiState (Just scrollable) Nothing usrMap catMap url) (VtyEvent ev) = continue . (\x -> TuiState (Just x) Nothing usrMap catMap url) =<< handler
    where
        handler :: EventM String (List String Topic)
        handler = handleListEvent ev $ scrollable
