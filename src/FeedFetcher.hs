{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module FeedFetcher (
  feedFetcher
) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LC8

import Conduit
import Control.Concurrent   (threadDelay)
import Control.Monad        (void, forM_)
import Database.Persist.Sql ((==.), ConnectionPool, Entity(..), selectList,
                             selectKeysList, insert, runSqlPool, fromSqlKey)
import Network.HTTP.Simple
import Network.HTTP.Types.Status

import Foundation
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
  -- FIXME: apparently this one can throw too (i.e. if the service is
  -- unreachable, which happened to me on flaky Wi-Fi); handle that
  response <- httpLBS (addUserAgentHeader request)

  let status = getResponseStatus response
  if status == ok200
    then return $ Right $ getResponseBody response
    else return $ Left status

getDeviationId :: Deviation -> DB DeviationId
getDeviationId deviation = do
  let link = deviationLink deviation
  existingIds <- selectKeysList [DeviationLink ==. link] []
  case existingIds of
    [key] -> return key
    []    -> insert deviation
    -- FIXME: this case is impossible because we either have that item or we
    -- don't; throw an error or something
    _     -> undefined

storeDeviations :: ConnectionPool -> [(Deviation, T.Text)] -> IO ()
storeDeviations sqlConnPool deviations =
  forM_ deviations $ \(deviation, previewUrl) -> do
    void $ (flip runSqlPool) sqlConnPool $ do
      deviationId <- getDeviationId deviation
      let imageNo = fromSqlKey deviationId

      -- FIXME: might throw, handle that; see similar situation above in
      -- fetchFeed
      request <- parseRequest $ T.unpack previewUrl
      -- FIXME: might throw, handle that; see similar situation above in
      -- fetchFeed
      let filename = concat ["previews/", show imageNo, ".jpg"]
      runResourceT $ httpSink request (const $ sinkFile filename)

feedFetcher :: ConnectionPool -> IO ()
feedFetcher sqlConnPool = do
  -- Five minutes is the setting in DeviantArt's cache-control at the moment.
  -- TODO: actually gather cache-control headers from responses and sleep for
  -- the *longest* duration specified in them
  threadDelay $ 5 * 60 * 1000 * 1000

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
