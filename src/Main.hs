module Main where

import HtmlFetcher (fetchHtml)

main :: IO ()
main = fetchHtml "https://example.com"

