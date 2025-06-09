{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module DataFetcher where
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple
import Data.List (sortBy, maximumBy, minimumBy)
import Data.Ord (comparing, Down(..))
import Text.Printf

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

data CurrencyAnalysis = CurrencyAnalysis
    { currencyCode :: String
    , currencyName :: String
    , currentRate :: Double
    , averageRate :: Double
    , minRate :: Double
    , maxRate :: Double
    , volatility :: Double
    , trend :: String
    , changePercent :: Double
    } deriving (Show)

currencies :: [String]
currencies = ["EUR", "USD", "CHF", "GBP", "JPY", "CAD", "AUD", "CNY", "HKD"]

numOfLastCurr :: Int
numOfLastCurr = 100

fetchData :: IO ()
fetchData = do
    putStrLn "=== FETCHING AND ANALYZING EXCHANGE RATES ==="
    analyses <- mapM fetchAndAnalyzeCurrency currencies
    putStrLn "\n=== ANALYSIS SUMMARY ==="
    printSummaryAnalysis analyses

fetchAndAnalyzeCurrency :: String -> IO CurrencyAnalysis
fetchAndAnalyzeCurrency curr = do
    let url = "https://api.nbp.pl/api/exchangerates/rates/A/" ++ curr ++ "/last/" ++ show numOfLastCurr ++ "/?format=json"
    request <- parseRequest url
    response <- httpLBS request
    let body = getResponseBody response
    case decode body :: Maybe ExchangeRates of
        Just ex -> do
            let analysis = analyzeCurrency ex
            printCurrencyData ex analysis
            return analysis
        Nothing -> do
            putStrLn $ "Error parsing data for: " ++ curr
            return $ CurrencyAnalysis curr "Unknown" 0 0 0 0 0 "Unknown" 0

analyzeCurrency :: ExchangeRates -> CurrencyAnalysis
analyzeCurrency ex = 
    let rateValues = map mid (rates ex)
        sortedRates = sortBy (comparing mid) (rates ex)
        minRate = mid $ head sortedRates
        maxRate = mid $ last sortedRates
        avgRate = sum rateValues / fromIntegral (length rateValues)
        sortedByDate = rates ex
        currentRate = mid $ head sortedByDate
        oldestRate = mid $ last sortedByDate
        changePercent = if oldestRate /= 0 
                       then ((currentRate - oldestRate) / oldestRate) * 100
                       else 0
        volatility = calculateVolatility rateValues avgRate
        trend = determineTrend (map mid sortedByDate)
    in CurrencyAnalysis
        { currencyCode = code ex
        , currencyName = currency ex
        , currentRate = currentRate
        , averageRate = avgRate
        , minRate = minRate
        , maxRate = maxRate
        , volatility = volatility
        , trend = trend
        , changePercent = changePercent
        }

calculateVolatility :: [Double] -> Double -> Double
calculateVolatility rates avg = 
    let variances = map (\r -> (r - avg) ** 2) rates
        variance = sum variances / fromIntegral (length variances)
    in sqrt variance

determineTrend :: [Double] -> String
determineTrend rates
    | length rates < 3 = "Insufficient data"
    | head rates > last rates = "Rising"
    | head rates < last rates = "Falling"
    | otherwise = "Stable"

printCurrencyData :: ExchangeRates -> CurrencyAnalysis -> IO ()
printCurrencyData ex analysis = do
    putStrLn $ "\n" ++ replicate 50 '='
    putStrLn $ "CURRENCY: " ++ currencyName analysis ++ " (" ++ currencyCode analysis ++ ")"
    putStrLn $ replicate 50 '='
    
    putStrLn "\n📊 DETAILED RATES:"
    mapM_ printRate (sortBy (comparing (Data.Ord.Down . effectiveDate)) (rates ex))
    
    putStrLn "\n📈 STATISTICAL ANALYSIS:"
    printf "  Current rate:      %.4f PLN\n" (currentRate analysis)
    printf "  Average rate:      %.4f PLN\n" (averageRate analysis)
    printf "  Minimum rate:      %.4f PLN\n" (minRate analysis)
    printf "  Maximum rate:      %.4f PLN\n" (maxRate analysis)
    printf "  Volatility (std):  %.4f PLN\n" (volatility analysis)
    printf "  Change %%:          %+.2f%%\n" (changePercent analysis)
    putStrLn $ "  Trend:             " ++ trend analysis

printRate :: Rate -> IO ()
printRate rate = printf "  %s: %.4f PLN\n" (effectiveDate rate) (mid rate)

printSummaryAnalysis :: [CurrencyAnalysis] -> IO ()
printSummaryAnalysis analyses = do
    putStrLn $ "\n" ++ replicate 60 '='
    putStrLn "CURRENCY RANKING"
    putStrLn $ replicate 60 '='
    
    putStrLn "\n🏆 HIGHEST RATE:"
    let highestRate = maximumBy (comparing currentRate) analyses
    printf "  %s: %.4f PLN\n" (currencyCode highestRate) (currentRate highestRate)
    
    putStrLn "\n📉 LOWEST RATE:"
    let lowestRate = minimumBy (comparing currentRate) analyses
    printf "  %s: %.4f PLN\n" (currencyCode lowestRate) (currentRate lowestRate)
    
    putStrLn "\n⚡ MOST VOLATILE:"
    let mostVolatile = maximumBy (comparing volatility) analyses
    printf "  %s: %.4f PLN (std deviation)\n" (currencyCode mostVolatile) (volatility mostVolatile)
    
    putStrLn "\n📊 BIGGEST GAINER:"
    let biggestGainer = maximumBy (comparing changePercent) analyses
    printf "  %s: %+.2f%%\n" (currencyCode biggestGainer) (changePercent biggestGainer)
    
    putStrLn "\n📉 BIGGEST LOSER:"
    let biggestLooser = minimumBy (comparing changePercent) analyses
    printf "  %s: %+.2f%%\n" (currencyCode biggestLooser) (changePercent biggestLooser)
    
    putStrLn "\n💹 RECOMMENDATIONS:"
    mapM_ printRecommendation analyses

printRecommendation :: CurrencyAnalysis -> IO ()
printRecommendation analysis = do
    let recommendation :: String
        recommendation 
            | changePercent analysis > 2.0 = "📈 CONSIDER SELLING"
            | changePercent analysis < -2.0 = "💰 CONSIDER BUYING"
            | otherwise = "⚖️  HOLD/WATCH"
    printf "  %s: %s (change: %+.2f%%)\n" 
           (currencyCode analysis) 
           recommendation 
           (changePercent analysis)

fetchDataForCurrency :: String -> IO ()
fetchDataForCurrency curr = do
    analysis <- fetchAndAnalyzeCurrency curr
    return ()

exportToCSV :: [CurrencyAnalysis] -> IO ()
exportToCSV analyses = do
    let csvHeader = "Currency,Current Rate,Average,Min,Max,Volatility,Change %,Trend\n"
    let csvRows = map analysisToCSV analyses
    writeFile "currency_analysis.csv" (csvHeader ++ unlines csvRows)
    putStrLn "✅ Data exported to currency_analysis.csv"

analysisToCSV :: CurrencyAnalysis -> String
analysisToCSV analysis = 
    currencyCode analysis ++ "," ++
    show (currentRate analysis) ++ "," ++
    show (averageRate analysis) ++ "," ++
    show (minRate analysis) ++ "," ++
    show (maxRate analysis) ++ "," ++
    show (volatility analysis) ++ "," ++
    show (changePercent analysis) ++ "," ++
    trend analysis
