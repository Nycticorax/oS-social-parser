{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}

module ServiceTypes where

import           Control.Monad (guard)
import           Data.Aeson
import           Data.Foldable (asum)
import qualified Data.Text     as T
import           Data.Time     (NominalDiffTime)
import qualified Data.Vector   as V
import           Text.Read     (readMaybe)
import           Time

data Service = Telegram String | Reddit String | Discord String | Forums String

-- TELEGRAM

data TgMessages = TgMessages { na :: T.Text, ms :: [TgMessage] }

data TgMessage = TgMessage { ty :: T.Text, da :: T.Text, txt :: T.Text, fi :: Integer }

instance FromJSON TgMessages where
    parseJSON = withObject "TgMessages" $ \o -> do
        na <- o .: "name"
        ms <- o.: "messages"
        return TgMessages{..}

instance FromJSON TgMessage where
    parseJSON = withObject "TgMessage" $ \o -> do
        ty <- o .: "type"
        da <- o .: "date" >>= \d -> case isoOrCustom d of
            Left _     -> return d
            Right date -> return $ T.pack . show $ date
        txt <- asum [
            o .: "text", do
            x <- o .: "text"
            case x of
                Array arr -> case V.head arr of
                    String s -> parseText s
                    _        -> return ""
                String s -> parseText s
            ]
        fi <- asum [
            o .: "from_id",
            do
                guard (ty == "service")
                return 0
            ]
        return TgMessage{..}

instance ToJSON TgMessage where
    toJSON TgMessage{..} = object [
        "date" .= da,
        "text" .= txt,
        "from_id" .= fi
        ]

-- DISCORD

data DiscMessages = DiscMessages { messages :: [DiscMessage] }

data DiscMessage = DiscMessage { auth :: DiscAuthor, t :: T.Text, timestamp :: T.Text, content :: T.Text }

data DiscAuthor = DiscAuthor { authid :: Integer }

instance FromJSON DiscMessages where
    parseJSON = withObject "DiscMessages" $ \o -> do
        messages <- o .: "messages"
        return DiscMessages{..}

instance FromJSON DiscMessage where
    parseJSON = withObject "DiscMessage" $ \o -> do
        auth <- o .: "author"
        t <- o .: "type"
        timestamp <- o .: "timestamp" >>= \d -> case isoOrCustom d of
            Left _     -> return d
            Right date -> return $ T.pack . show $ date
        content <- o.: "content"
        return DiscMessage{..}

instance FromJSON DiscAuthor where
    parseJSON = withObject "DiscAuthor" $ \o -> do
        authid <- o .: "id" >>= \n -> case readMaybe n of
            Just i  -> return i
            Nothing -> return 0
        return DiscAuthor{..}

instance ToJSON DiscMessage where
    toJSON DiscMessage{..} = object [
        "from_id" .= authid auth,
        "content" .= content,
        "date" .= timestamp
        ]

data RedditMessages = RedditMessages { reddmess :: [RedditMessage] }

data RedditMessage = RedditMessage { selftext :: T.Text, created_utc :: T.Text, author :: T.Text, title :: T.Text }

instance FromJSON RedditMessages where
    parseJSON = withObject "RedditMessages" $ \o -> do
        reddmess <- o .: "data"
        return RedditMessages{..}

instance FromJSON RedditMessage where
    parseJSON = withObject "RedditMessage" $ \o -> do
        selftext <- o .: "selftext"
        created_utc <- o .: "created_utc" >>= \d -> return . T.pack . show . unixToUTC $ d
        author <- o .: "author"
        title <- o .: "title"
        return RedditMessage{..}

instance ToJSON RedditMessage where
    toJSON RedditMessage{..} = object [
        "from_id" .= author,
        "content" .= selftext,
        "date" .= created_utc,
        "title" .= title
        ]

parseText s = if T.null s then return "" else return s

data ForumsPost = ForumsPost { postText :: T.Text, postDate :: T.Text , postTitle :: T.Text, postThreadId :: T.Text , postParentThreadId :: T.Text , postAuthorId :: T.Text  } deriving (Show)

instance ToJSON ForumsPost where
    toJSON ForumsPost{..} = object [
        "from_id" .= postAuthorId,
        "content" .= postText,
        "date" .= postDate,
        "title" .= postTitle,
        "thread_id" .= postThreadId,
        "parent_thread_id" .= postParentThreadId
        ]
