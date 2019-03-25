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
import qualified Data.Text as T
import Text.Pandoc
import Text.Pandoc.Writers.CommonMark
import Text.Pandoc.Readers.HTML
import System.Environment
import Types
import System.Exit

helpMessage = "Usage: discourse-tui url \n Ex: discourse-tui http://discourse.haskell.org"

parseArgs :: IO String
parseArgs = do
    args <- getArgs
    if null args || (head args) == "--help"
        then die helpMessage
        else return $ head args

main :: IO ()
main = do
    baseUrl <- parseArgs
    initialState <- getTuiState baseUrl
    endState <- defaultMain tuiApp initialState
    putStrLn "close"

-- initialize the TuiState with the list of topics and catagories
getTuiState :: String -> IO TuiState
getTuiState baseUrl = do
    topicsRequest <- parseRequest (baseUrl ++ "/latest.json")
    categoriesRequest <- parseRequest (baseUrl ++ "/categories.json")
    (TopicResponse users topicList) <- getResponseBody <$> httpJSON topicsRequest
    categoriesResp <- getResponseBody <$> httpJSON categoriesRequest
    return TuiState {
                    posts = Nothing,
                    topics = list "contents" (V.fromList topicList) topicHeight,
                    userMap = M.fromList . map (\x -> (userId x, x)) $ users,
                    categoryMap = M.fromList . map (\x -> (categoryId x, x)) . categories $ categoriesResp,
                    baseURL = baseUrl,
                    singlePostView = False
                    }

-- the help bar at the bottom
helpBar :: Widget String
helpBar = withAttr "bar" . str $ "arrow keys -> move | left right -> read replies/full post | q to quit"

-- get the posts for the current topic
getPosts :: TuiState -> IO (List String Post)
getPosts (TuiState {baseURL = baseURL, topics = topics}) = do
    let (Just selectedTopicID) = topicId . snd <$> listSelectedElement topics 
    postsRequest <- parseRequest $ baseURL ++ "/t/" ++ (show selectedTopicID) ++ ".json"
    (PostResponse posts') <- getResponseBody <$> httpJSON postsRequest
    posts <- mapM postToPandoc posts'
    return $ list "posts" (V.fromList posts) 10


postToPandoc posts = do
    newContents <- toMarkdown . contents $ posts
    return posts {contents = newContents} 


type ResourceName = String

tuiApp :: App TuiState e ResourceName
tuiApp =
    App
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure
        , appAttrMap = const $ attrMap mempty [("title", withStyle currentAttr bold), ("pinned", fg green), ("selected", fg red), ("OP", fg blue), ("rest", defAttr), ("bar", fg yellow)]
        }

toMarkdown :: String -> IO String
toMarkdown s = do
    result <- runIO $ do
        doc <- readHtml def (T.pack s)
        writeCommonMark def doc
    rst <- handleError result
    return $ T.unpack rst

-- draws the entire TuiState
-- this pattern matches the topic list
drawTui :: TuiState -> [Widget ResourceName]
drawTui (TuiState scrollable Nothing userMap categoryMap _ _) = (:[]) $ (renderList drawTopic True $ scrollable) <=> helpBar
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
-- this pattern matches the post list
drawTui (TuiState _ (Just posts) _ _ _ False)
    = (:[])
    $ (renderList drawPost True $ posts)
    <=> helpBar
    where
        drawPost selected (Post id username' contents score')
            = border'
            . withAttr (if selected then "selected" else "")
            $ (hLimit 4 . padRight Max . str . show $ score')
            <+> (userName''
            <=> contents')
            where
                userName'' = withAttr "OP" . str $ username'
                contents' = strWrap contents
                border' = border
                        . vLimit 8
                        . padBottom Max
                        . padRight  Max

drawTui (TuiState {posts = (Just posts), singlePostView = True}) = (:[]) $ case listSelectedElement posts of
    (Just (_, post)) -> (withAttr "OP" . str . opUserName $ post)
     <=> padBottom Max (str . contents $ post) <=> helpBar
    Nothing -> str "something went wrong"

mapFst :: (a -> a) -> (a -> a) ->  [a] -> [a]
mapFst fn fn' (x:xs) = (fn x) : (map fn' xs)

-- handles key presses. TODO: clean
handleTuiEvent :: TuiState -> BrickEvent String e -> EventM String (Next TuiState)
handleTuiEvent tui (VtyEvent (EvKey (KChar 'q') _)) = halt tui
handleTuiEvent (TuiState topics (Just list) usrMap catMap url singlePostView) (VtyEvent (EvKey KRight _)) = continue $ TuiState topics (Just list) usrMap catMap url (not singlePostView)
handleTuiEvent (TuiState topics (Just list) usrMap catMap url True) (VtyEvent (EvKey _ _)) = continue $ TuiState topics (Just list) usrMap catMap url False
handleTuiEvent tui (VtyEvent (EvKey KRight  _)) = do
    posts' <- liftIO $ getPosts tui
    continue $ tui {posts = Just posts'}

handleTuiEvent tui (VtyEvent (EvKey KLeft   _)) = continue $ tui {posts = Nothing}
handleTuiEvent (TuiState list Nothing usrMap catMap url spv) ev = scrollHandler (\x -> TuiState x Nothing usrMap catMap url spv) list ev
handleTuiEvent (TuiState topics (Just list) usrMap catMap url spv) ev = scrollHandler (\x -> TuiState topics (Just x) usrMap catMap url spv) list ev

scrollHandler restoreTuiState list (VtyEvent ev) = continue . restoreTuiState =<< handler
    where
        handler = handleListEvent ev list
