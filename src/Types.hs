module Types where

import Data.Aeson
import Cursor.Simple.List.NonEmpty (NonEmptyCursor)
import qualified Data.IntMap.Strict as M
import Brick.Widgets.List
import GHC.Generics
import Control.Lens
import Control.Lens.TH

instance FromJSON ProtoTopic where
    parseJSON = withObject "ProtoTopic" $ \v -> do 
        topicId' <- v .: "id"
        title'  <- v .: "title"
        likeCount' <- v.: "like_count"
        postsCount' <- v.: "posts_count"
        posters' <- v.: "posters"
        pinned' <- v.: "pinned"
        categoryId' <- v.: "category_id"
        return $ ProtoTopic topicId' categoryId' title' likeCount' postsCount' posters' pinned'

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

instance FromJSON Category where
    parseJSON (Object v) = Category
        <$> v .: "id"
        <*> v .: "name"

instance FromJSON CategoryResponse where
    parseJSON = withObject "CategoryResponse" $ \v -> do
        categoryList <- v .: "category_list"
        categories' <- categoryList .: "categories"
        return $ CategoryResponse categories'

instance FromJSON PostResponse where
    parseJSON = withObject "PastResponse" $ \v -> do
        postStream <- v .: "post_stream"
        posts' <- postStream .: "posts"
        return $ PostResponse posts'

instance FromJSON Post where
    parseJSON = withObject "Post" $ \v -> do
        id' <- v .: "id"
        username' <- v .: "username"
        cooked' <- v .: "cooked"
        actions <- v .: "actions_summary"
        return $ Post id' username' cooked' (if null actions then 0 else _count . head $ actions)

instance FromJSON Action where
    parseJSON (Object v) = Action
        <$> v .: "id"
        <*> v .: "count"

newtype CategoryResponse = CategoryResponse
    {
    _categories :: [Category]
    } deriving (Show)

data Action = Action
    {
    _actionId :: Int,
    _count :: Int
    } deriving (Show)

data TopicResponse = TopicResponse
    {
    _users :: [User],
    _topicList :: [ProtoTopic]
    } deriving (Show)

topicHeight :: Int
topicHeight = 4

data ProtoTopic = ProtoTopic
    {
    _topicId :: Int,
    _categoryID :: Int,
    _title :: String,
    _likeCount :: Int,
    _postsCount :: Int,
    _posters :: [Poster],
    _pinned :: Bool
    } deriving (Show)

data Topic = Topic
    {
    _topicId :: Int,
    _category :: String,
    _title :: String,
    _likeCount :: Int,
    _postsCount :: Int,
    _posters :: [String],
    _pinned :: Bool
    } deriving (Show)

data User = User
    {
    _userId :: Int,
    _userName :: String,
    _realName :: String
    } deriving (Show)


data Category = Category
    {
    _categoryId :: Int,
    _categoryName :: String
    } deriving (Show)

data Poster = Poster
    {
    _posterId :: Int,
    _description :: String
    } deriving (Show)

data PostResponse = PostResponse
    {
    _postList :: [Post]                         
    } deriving (Show)

data Post = Post
    {
    _postId :: Int,
    _opUserName :: String,
    _contents :: String,
    _likes :: Int
    } deriving (Show)

data TuiState = TuiState
    {
    _topics :: List String Topic,
    _posts :: Maybe (List String Post), -- Nothing if not in post view
    _baseURL :: String,
    _singlePostView :: Bool -- if we're looking at the full contents of one post
    } deriving (Show)

type ResourceName = String

makeLenses ''CategoryResponse
makeLenses ''Post
makeLenses ''PostResponse
makeLenses ''Category
makeLenses ''Poster
makeLenses ''Topic
makeLenses ''TopicResponse
makeLenses ''Action
makeLenses ''User
makeLenses ''TuiState
