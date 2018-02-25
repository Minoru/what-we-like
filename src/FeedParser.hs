{-# LANGUAGE OverloadedStrings #-}

module FeedParser (
  parse
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LC8

import Text.Feed.Import
import Text.Feed.Query
import Text.RSS.Syntax
import Text.Feed.Types
import Data.XML.Types (Element(..), Name(..), Content(ContentText))
import Data.Maybe (fromMaybe, isJust)

import Model (Deviation(..))

isMediumImage :: [(Name, [Content])] -> Bool
isMediumImage [] = False
isMediumImage ((name, [ContentText value]):xs) =
  if nameLocalName name == T.pack "medium" && value == "image"
  then True
  else isMediumImage xs
isMediumImage (_:xs) = isMediumImage xs

isMediaContent :: Element -> Bool
isMediaContent e =
  let name = elementName e
      attrs = elementAttributes e
  in  nameLocalName name == T.pack "content"
      && nameNamespace name == (Just $ T.pack "http://search.yahoo.com/mrss/")
      && isMediumImage attrs

isUrl :: Name -> Bool
isUrl n = nameLocalName n == T.pack "url"

extractDeviation :: Item -> Maybe (Deviation, T.Text)
extractDeviation (Text.Feed.Types.RSSItem item) =
  let
    link = rssItemLink item
    title = fromMaybe "" $ rssItemTitle item
    author = fromMaybe "" $ rssItemAuthor item

    attr =
        concatMap snd
      $ filter (isUrl . fst)
      $ concatMap elementAttributes
      $ filter isMediaContent (rssItemOther item)
  in case attr of
       [ContentText previewUrl] -> link >>= \itemLink -> do
         return $ (Deviation itemLink title author, previewUrl)
       _ -> Nothing
extractDeviation _ = Nothing

parse :: LC8.ByteString -> [(Deviation, T.Text)]
parse source =
  case parseFeedSource source of
    Just feed -> do
      case feed of
        (RSSFeed _) ->
          let items = getFeedItems feed
          in  case (sequence $ filter isJust $ map extractDeviation items) of
                Just list -> take 3 $ list
                Nothing -> []

        _ -> []

    Nothing -> []
