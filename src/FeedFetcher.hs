{-# LANGUAGE OverloadedStrings #-}

module FeedFetcher (
  feedFetcher
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LC8

import Control.Concurrent   (threadDelay)
import Control.Monad        (void, forM_)
import Database.Persist.Sql (ConnectionPool, Entity(..), selectList, insert,
                             runSqlPool)
import Network.HTTP.Simple
import Network.HTTP.Types.Status

import Model
import FeedParser

getFeedsList :: ConnectionPool -> IO [Entity RSSFeed]
getFeedsList = runSqlPool (selectList [] [])

addUserAgentHeader :: Request -> Request
addUserAgentHeader =
  addRequestHeader
    "User-Agent"
    "Mozilla/5.0 (X11; Linux x86_64; rv:58.0) Gecko/20100101 Firefox/58.0"

fetchFeed :: T.Text -> IO (Either Status LC8.ByteString)
fetchFeed url = do
  -- FIXME: parseRequest might throw, handle that
  request <- parseRequest $ T.unpack url
  response <- httpLBS (addUserAgentHeader request)

  let status = getResponseStatus response
  if status == ok200
  then return $ Right $ getResponseBody response
  else return $ Left status

storeDeviations :: ConnectionPool -> [Deviation] -> IO ()
storeDeviations sqlConnPool deviations =
  void $ runSqlPool (mapM_ insert deviations) sqlConnPool

feedFetcher :: ConnectionPool -> IO ()
feedFetcher sqlConnPool = do
  threadDelay 30000000 -- wait thirty seconds

  feeds <- getFeedsList sqlConnPool
  forM_ feeds $ \(Entity _ (RSSFeed _ url)) -> do
    feed <- fetchFeed url
    case feed of
      Right feed' -> do
        let deviations = parse feed'
        storeDeviations sqlConnPool deviations

      -- TODO: log the error
      Left _status -> return ()

  feedFetcher sqlConnPool
