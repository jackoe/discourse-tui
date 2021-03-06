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
import Control.Lens

-- change a protoTopic into a topic by consulting userMap and catagoryMap
parseTopic :: M.IntMap User -> M.IntMap Category -> ProtoTopic -> Topic
parseTopic userMap catagoryMap (ProtoTopic topicId catId title likeC postsC posters pinned)
    = Topic {
        _title = title,
        _topicId = topicId,
        _category = ((catagoryMap M.! catId) ^. categoryName),
        _likeCount = likeC,
        _postsCount = postsC,
        _posters = (map (\x -> (userMap M.! (x ^. posterId)) ^. userName) posters),
        _pinned = pinned
            }

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
    return ()

-- initialize the TuiState with the list of topics and catagories
getTuiState :: String -> IO TuiState
getTuiState baseUrl = do
    topicsRequest <- parseRequest (baseUrl ++ "/latest.json")
    categoriesRequest <- parseRequest (baseUrl ++ "/categories.json")
    categoriesResp <- getResponseBody <$> httpJSON categoriesRequest
    (TopicResponse users topicList) <- getResponseBody <$> httpJSON topicsRequest
    let userMap     = M.fromList . map (\x -> (x ^. userId, x)) $ users
    let categoryMap = M.fromList . map (\x -> (x ^. categoryId, x)) $ categoriesResp ^. categories
    return TuiState {
        _posts = Nothing,
        _topics = list "contents" (V.fromList $ map (parseTopic userMap categoryMap) topicList) topicHeight,
        _baseURL = baseUrl,
        _singlePostView = False
                    }

-- the help bar at the bottom
helpBar :: Widget String
helpBar = withAttr "bar" . str $ "arrow keys -> move | left right -> read replies/full post | q to quit"

-- get the posts for the current topic
getPosts :: TuiState -> IO (List String Post)
getPosts ts = do
    let (Just selectedTopicID) = view (_2 . topicId) <$> listSelectedElement (ts ^. topics)
    postsRequest <- parseRequest $ mconcat [ts^.baseURL, "/t/", show selectedTopicID, ".json"]
    (PostResponse posts') <- getResponseBody <$> httpJSON postsRequest
    posts <- mapM postToPandoc posts'
    return $ list "posts" (V.fromList posts) 10

postToPandoc :: Post -> IO Post
postToPandoc post = do
    newContents <- toMarkdown $ post ^. contents
    return $ post & contents .~ newContents



tuiApp :: App TuiState e ResourceName
tuiApp =
    App
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure
        , appAttrMap = const $ attrMap mempty [("title", withStyle defAttr bold), ("pinned", fg green), ("selected", bg yellow), ("OP", fg blue), ("rest", defAttr), ("bar", fg yellow)]
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
drawTui (TuiState scrollable Nothing _ _) = (:[]) $ (renderList drawTopic True $ scrollable) <=> helpBar
    where
        drawTopic selected (Topic _ category' title likeCount postsCount posters pinned)
                        = border
                        . (if pinned   then  withAttr "pinned"   else id)
                        . padRight Max
                        $ (likes <+> title') <=> hBox [category, postsCount', posters']
            where
                likes :: Widget ResourceName
                likes = (if selected then  withAttr "selected" else id)
                      . padRight (Pad 1)
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
                       $ posters

                category :: Widget ResourceName
                category = padLeft (Pad 5) . str $ category'

                showList :: [String] -> [Widget ResourceName]
                showList s = map str $ (map (++ " ") . init $ s) ++ [last s]

-- this pattern matches the post list
drawTui (TuiState _ (Just posts) _ False)
    = (:[])
    $ (renderList drawPost True $ posts)
    <=> helpBar
    where
        drawPost selected (Post id username' contents score')
            = border'
            $ withAttr (if selected then "selected" else "")
              (hLimit 4 . padRight Max . str . show $ score') 
            <+> (userName''
            <=> contents')
            where
                userName'' = withAttr "OP" . str $ username'
                contents' = strWrap contents
                border' = border
                        . vLimit 8
                        . padBottom Max
                        . padRight  Max

drawTui (TuiState _ (Just posts) _ True) = (:[]) $ case listSelectedElement posts of
    (Just (_, post)) -> (withAttr "OP" . str $ post ^. opUserName)
     <=> padBottom Max (str $ post ^. contents) <=> helpBar
    Nothing -> str "something went wrong"

mapFst :: (a -> a) -> (a -> a) ->  [a] -> [a]
mapFst fn fn' (x:xs) = (fn x) : (map fn' xs)

handleTuiEvent :: TuiState -> BrickEvent String e -> EventM String (Next TuiState)
handleTuiEvent tui (VtyEvent (EvKey (KChar 'q') _)) = halt tui

handleTuiEvent (TuiState topics (Just list) url singlePostView) (VtyEvent (EvKey KRight _))
    = continue $ TuiState topics (Just list) url (not singlePostView)

handleTuiEvent (TuiState topics (Just list) url True) (VtyEvent (EvKey _ _))
      = continue $ TuiState topics (Just list) url False

handleTuiEvent tui (VtyEvent (EvKey KRight  _)) = do
    posts' <- liftIO $ getPosts tui
    continue $ tui & posts .~ (Just posts')

handleTuiEvent tui (VtyEvent (EvKey KLeft   _))
    = continue $ tui & posts .~ Nothing

handleTuiEvent (TuiState list Nothing url spv) ev
    = scrollHandler (\x -> TuiState x Nothing url spv) list ev

handleTuiEvent (TuiState topics (Just list) url spv) ev
    = scrollHandler (\x -> TuiState topics (Just x) url spv) list ev

scrollHandler restoreTuiState list (VtyEvent ev) = continue . restoreTuiState =<< handler
    where
        handler = handleListEvent ev list
