{-# LANGUAGE OverloadedStrings #-}

module Parser (parseFromDir) where

import           Control.Exception
import           Data.Aeson
import           Data.Foldable           (traverse_)
import qualified Data.Map                as M
import qualified Data.Text               as T
import           ServiceTypes
import           System.Directory
import           System.IO
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match (anyAttrValue, tagOpenAttrNameLit,
                                          tagOpenLit)
import           Text.Printf             (printf)

tgParsing msgs output_path = do
    case msgs of
        Left err -> parseErr err
        Right res -> do
            let filtered = tg_post_filter . ms $ res
            encodeFile output_path filtered
            parsedOK filtered
    where   tg_post_filter = filter (\m -> (ty m == "message") && not (T.null $ txt m) )

discordParsing msgs output_path = do
    case msgs of
        Left err -> parseErr err
        Right res -> do
            let filtered = disc_post_filter $ messages res
            encodeFile output_path filtered
            parsedOK $ messages res
    where   disc_post_filter = filter (\m -> t m == "Default")

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

parseFileWithXML dir file =
    let file_short = T.unpack . head . T.splitOn "." . T.pack $ file
        output_path = dir ++ file_short ++ "_cleaned.json"
        file_long = file_short ++ ".xml"
    in  do
        printf "\nParsing '%s' in dir %s to '%s'..." file_short dir output_path
        contents <- readFile $ dir ++ file_long
        encodeFile output_path $ parseXML contents
        printf "\nDone %s" output_path
    where   parseXML c =
                let attrVals = ["threadid", "parentid", "userid", "title", "dateline", "pagetext"] :: [String]
                    makeRows = partitions (~== ("<row>" :: String)) . parseTags
                    makeFields = partitions $ tagOpenLit "field" $ anyAttrValue (`elem` attrVals)
                    extract = head . map fromTagText . filter isTagText
                    toForumsPost fs =
                        let [tid, pid, uid, title, dateline, ptext] = map T.pack fs
                        in  ForumsPost ptext dateline title tid pid uid
                in map (toForumsPost . map extract . makeFields) . makeRows $ c

partitionsMap pred = M.fromList . preMap . partitions pred
    where   preMap [] = []
            preMap (tl:tls) =
                let (k, v) = (getTagName . head $ tl, tl)
                in  (k, v) : preMap tls
            getTagName (TagOpen n _) = n

partitionsMap' pred = M.fromList . preMap' . partitions pred
    where   preMap' [] = []
            preMap' (tl: tls) =
                let (t:ts) = init tl
                    (k, v) = (getTagName t, ts)
                in  (k, v) : preMap' tls
                where   getTagName (TagOpen n _) = n

parseErr err = print $ "\nChoked on something: " ++ err

parsedOK msgs = printf "\nSuccessfully re-encoded %s messages." (show . length $ msgs)

callService :: String -> String -> Service
callService dir parseService
    |   null parseService = throw $ userError "Directory or service not specified."
    |   parseService == "reddit" = Reddit dir
    |   parseService == "telegram" = Telegram dir
    |   parseService =="discord" = Discord dir
    |   parseService == "forums" = Forums dir
    |   otherwise = throw $ userError "Incorrect service (accepts only 'reddit', 'telegram', 'discord'"

parseFromDir dir parseService = do
    files <- listDirectory dir
    if null files then throw $ userError "No file in the specified directory."
    else case callService dir parseService of
        Reddit d   -> traverse_ (parseFileWith redditParsing d) files
        Telegram d -> traverse_ (parseFileWith tgParsing d) files
        Discord d  -> traverse_ (parseFileWith discordParsing d) files
        Forums d   -> traverse_ (parseFileWithXML d) files
    print "\nExiting."
