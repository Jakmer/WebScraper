{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DataFetcher where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

data Rate = Rate
    { no :: String
    , effectiveDate :: String
    , mid :: Double
    } deriving (Show, Generic)

instance FromJSON Rate

data ExchangeRates = ExchangeRates
    { table :: String
    , currency :: String
    , code :: String
    , rates :: [Rate]
    } deriving (Show, Generic)

instance FromJSON ExchangeRates

currencies :: [String]
currencies = ["EUR", "USD", "CHF", "GBP"]
numOfLastCurr = 10

fetchData :: IO ()
fetchData = mapM_ fetchDataForCurrency currencies

fetchDataForCurrency :: String -> IO ()
fetchDataForCurrency curr = do
    let url = "https://api.nbp.pl/api/exchangerates/rates/A/" ++ curr ++ "/last/" ++ show numOfLastCurr ++ "/?format=json"
    request <- parseRequest url
    response <- httpLBS request
    let body = getResponseBody response
    case decode body :: Maybe ExchangeRates of
        Just ex -> do
            putStrLn $ "\nCurrency: " ++ currency ex ++ " (" ++ code ex ++ ")"
            mapM_ printRate (rates ex)
        Nothing -> putStrLn $ "Error parsing data for: " ++ curr

printRate :: Rate -> IO ()
printRate rate = putStrLn $
    effectiveDate rate ++ " - " ++ show (mid rate)
