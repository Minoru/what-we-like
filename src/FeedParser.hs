module FeedParser (
  parse
) where

import qualified Data.ByteString.Lazy.Char8 as LC8

import Model

parse :: LC8.ByteString -> [Deviation]
parse feed = undefined
