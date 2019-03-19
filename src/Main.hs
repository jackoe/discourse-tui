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
import Brick.Widgets.Border
import qualified Data.IntMap.Strict as M
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
                    cursor = topicListToCursor . topicList $ topicsResp,
                    userMap = M.fromList . map (\x -> (userId x, x)) . users $ topicsResp,
                    categoryMap = M.fromList . map (\x -> (categoryId x, x)) . categories $ categoriesResp
                    }

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
drawTui (TuiState (NE.NonEmptyCursor prev curr next) userMap _) = (:[]) . viewport "posts" Vertical . vBox $
        (map drawPost . reverse $ prev)
     ++ (withAttr "selected" .  drawPost $ curr)
      : (map drawPost next)
    where
        drawPost topic = border
                        . (if (pinned topic) then  withAttr "pinned" else id)
                        . padRight Max
                        $ (likes <+> title') <=> (postsCount' <+> poster)
            where
                likes :: Widget ResourceName
                likes = padRight (Pad 1)
                      . hLimit 4
                      . padRight Max
                      $ (str . show . likeCount $ topic)

                title' :: Widget ResourceName
                title' = withAttr "title" . str . title $ topic

                postsCount' :: Widget ResourceName
                postsCount' = padLeft (Pad 5)
                            . str
                            . ("posts: " ++)
                            . show
                            . postsCount
                            $ topic

                poster :: Widget ResourceName
                poster = padLeft (Pad 5)
                       . hBox
                       . mapFst (withAttr "OP") (withAttr "rest")
                       . showList
                       . map (\x ->  userName $ userMap M.! posterId x)
                       . posters
                       $ topic

                showList :: [String] -> [Widget ResourceName]
                showList s = map str $ (map (++ " ") . init $ s) ++ [last s]

                mapFst :: (a -> a) -> (a -> a) ->  [a] -> [a]
                mapFst fn fn' (x:xs) = (fn x) : (map fn' xs)

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s (VtyEvent (EvKey key [])) = scroll s key 
handleTuiEvent s e = halt s

scroll s key = continue s {
                          cursor = fromMaybe (cursor s) $ ((case key of
                              KDown -> SNE.nonEmptyCursorSelectNext
                              KUp   -> SNE.nonEmptyCursorSelectPrev) (cursor s))
                      }

topicListToCursor :: [Topic] -> NonEmptyCursor Topic
topicListToCursor (x:xs) = NE.NonEmptyCursor [] x xs
