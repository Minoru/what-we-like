{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Page where

import Import


import Database.Persist.Sql (fromSqlKey)

resultsPerPage :: Int
resultsPerPage = 10

getPageR :: Int -> Handler Html
getPageR pageNumber = do
    candidates <- runDB $
      selectList
        []
        [ Desc DeviationId
        -- asking for one more result so that we can detect if there's anything
        -- on the next page
        , LimitTo (resultsPerPage + 1)
        , OffsetBy $ (pageNumber - 1) * resultsPerPage
        ]
    let (deviations, lastOne) = splitAt resultsPerPage candidates
    let paginationWidget =
          if null lastOne
            then $(widgetFile "the-end")
            else let next = pageNumber + 1 in $(widgetFile "pagination")
    defaultLayout $ do
        setTitle "What We Like"
        $(widgetFile "homepage")
