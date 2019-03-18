module Types where

import Data.Aeson
import Cursor.Simple.List.NonEmpty (NonEmptyCursor)
import qualified Data.IntMap.Strict as M


instance FromJSON Topic where
    parseJSON = withObject "Topic" $ \v -> do 
        postId' <- v .: "id"
        title'  <- v .: "title"
        likeCount' <- v.: "like_count"
        postsCount' <- v.: "posts_count"
        posters' <- v.: "posters"
        pinned' <- v.: "pinned"
        return $ Topic postId' title' likeCount' postsCount' posters' pinned'

instance FromJSON User where
    parseJSON (Object v) = User
            <$> v .: "id"
            <*> v .: "username"
            <*> v .: "name"

instance FromJSON TopicResponse where
    parseJSON = withObject "TopicResponse" $ \v -> do
           users' <- v .: "users"
           topicList' <- v .: "topic_list"
           topics' <- topicList' .: "topics"
           return $ TopicResponse users' topics'


instance FromJSON Poster where
    parseJSON (Object v) = Poster
           <$> v .: "user_id"
           <*> v .: "description"

data TopicResponse = TopicResponse
    {
    users :: [User],
    topicList :: [Topic]
    } deriving (Show)

data Topic = Topic {
                   postId :: Int,
                   title :: String,
                   likeCount :: Int,
                   postsCount :: Int,
                   posters :: [Poster],
                   pinned :: Bool
                   } deriving (Show)

data User = User {
                 userId :: Int,
                 userName :: String,
                 realName :: String
                 } deriving (Show)

data Poster = Poster {
                     posterId :: Int,
                     description :: String
                     } deriving (Show)

data TuiState =
    TuiState {
              cursor :: NonEmptyCursor Topic,
              userMap :: M.IntMap User
             }
    deriving (Show)
