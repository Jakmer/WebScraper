{-# LANGUAGE OverloadedStrings #-}

module HtmlFetcher where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

fetchHtml :: String -> IO ()
fetchHtml url = do
    response <- httpLBS (parseRequest_ url)
    let body = getResponseBody response
    L8.putStrLn body

