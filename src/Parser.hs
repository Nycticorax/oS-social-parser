{-# LANGUAGE OverloadedStrings #-}

module Parser (parseFromDir) where

import           Control.Exception
import           Data.Aeson
import           Data.Foldable     (traverse_)
import qualified Data.Text         as T
import           ServiceTypes
import           System.Directory
import           Text.Printf       (printf)

tgParsing msgs output_path = do
    case msgs of
        Left err -> parseErr err
        Right res -> do
            let filtered = tg_post_filter . ms $ res
            encodeFile output_path filtered
            parsedOK filtered
    where   tg_post_filter = filter (\m -> (ty m /= "service") && not (T.null $ txt m) )

discordParsing msgs output_path = do
    case msgs of
        Left err -> parseErr err
        Right res -> do
            encodeFile output_path $ messages res
            parsedOK $ messages res

redditParsing msgs output_path = do
    case msgs of
        Left err -> parseErr err
        Right res -> do
            encodeFile output_path $ reddmess res
            parsedOK $ reddmess res

parseFileWith parsing dir file =
    let file_short = T.unpack . head . T.splitOn "." . T.pack $ file
        output_path = dir ++ file_short ++ "_cleaned.json"
        file_long = file_short ++ ".json"
    in  do
        printf "\nParsing '%s' in dir %s to '%s'..." file_short dir output_path
        mb_messages <- eitherDecodeFileStrict $ dir ++ file_long
        parsing mb_messages output_path
        printf "\nDone %s" output_path

parseErr err = print $ "\nChoked on something: " ++ err

parsedOK msgs = printf "\nSuccessfully re-encoded %s messages." (show . length $ msgs)

callService :: String -> String -> Service
callService dir parseService
    |   null parseService = throw $ userError "Directory or service not specified."
    |   parseService == "reddit" = Reddit dir
    |   parseService == "telegram" = Telegram dir
    |   parseService =="discord" = Discord dir
    |   otherwise = throw $ userError "Incorrect service (accepts only 'reddit', 'telegram', 'discord'"

parseFromDir dir parseService = do
    files <- listDirectory dir
    if null files then throw $ userError "No file in the specified directory."
    else case callService dir parseService of
        Reddit d   -> traverse_ (parseFileWith redditParsing d) files
        Telegram d -> traverse_ (parseFileWith tgParsing d) files
        Discord d  -> traverse_ (parseFileWith discordParsing d) files
    print "\nExiting."