{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DataFetcher where

import GHC.Generics
import Data.Aeson
import Network.HTTP.Simple
import Data.List (sortBy, maximumBy, minimumBy)
import Data.Ord (comparing, Down(..))
import Text.Printf
import System.Directory (createDirectoryIfMissing)
import Analytics
import Charts

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
    createDirectoryIfMissing True "results"
    
    analyses <- mapM fetchAndAnalyzeCurrency currencies
    allRatesData <- mapM fetchRatesForAnalytics currencies
    dateRatesData <- mapM fetchDateRatesForCharts currencies
    
    putStrLn "\n=== BASIC ANALYSIS SUMMARY ==="
    printSummaryAnalysis analyses
    
    putStrLn "\n=== ADVANCED ANALYTICS ==="
    let advancedReport = generateAdvancedReport allRatesData
    mapM_ putStrLn advancedReport
    
    putStrLn "\n=== GENERATING CHARTS ==="
    generateAllCharts allRatesData dateRatesData
    
    exportToCSV analyses
    exportAdvancedAnalysis allRatesData
    
    putStrLn "\n✅ Analysis complete! Check 'results/' directory for output files."

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

fetchRatesForAnalytics :: String -> IO (String, [Double])
fetchRatesForAnalytics curr = do
    let url = "https://api.nbp.pl/api/exchangerates/rates/A/" ++ curr ++ "/last/" ++ show numOfLastCurr ++ "/?format=json"
    request <- parseRequest url
    response <- httpLBS request
    let body = getResponseBody response
    case decode body :: Maybe ExchangeRates of
        Just ex -> do
            let rateValues = reverse $ map mid (rates ex)
            return (curr, rateValues)
        Nothing -> do
            putStrLn $ "Error parsing data for analytics: " ++ curr
            return (curr, [])

analyzeCurrency :: ExchangeRates -> CurrencyAnalysis
analyzeCurrency ex = 
    let rateValues = map mid (rates ex)
        sortedRates = sortBy (comparing mid) (rates ex)
        minRateValue = mid $ head sortedRates
        maxRateValue = mid $ last sortedRates
        avgRate = sum rateValues / fromIntegral (length rateValues)
        sortedByDate = rates ex
        currentRateValue = mid $ head sortedByDate
        oldestRate = mid $ last sortedByDate
        changePercentValue = if oldestRate /= 0 
                       then ((currentRateValue - oldestRate) / oldestRate) * 100
                       else 0
        volatilityValue = calculateVolatility rateValues avgRate
        trendValue = determineTrend (map mid sortedByDate)
    in CurrencyAnalysis
        { currencyCode = code ex
        , currencyName = currency ex
        , currentRate = currentRateValue
        , averageRate = avgRate
        , minRate = minRateValue
        , maxRate = maxRateValue
        , volatility = volatilityValue
        , trend = trendValue
        , changePercent = changePercentValue
        }

calculateVolatility :: [Double] -> Double -> Double
calculateVolatility rateList avg = 
    let variances = map (\r -> (r - avg) ** 2) rateList
        variance = sum variances / fromIntegral (length variances)
    in sqrt variance

determineTrend :: [Double] -> String
determineTrend rateList
    | length rateList < 3 = "Insufficient data"
    | head rateList > last rateList = "Rising"
    | head rateList < last rateList = "Falling"
    | otherwise = "Stable"

printCurrencyData :: ExchangeRates -> CurrencyAnalysis -> IO ()
printCurrencyData ex analysis = do
    putStrLn $ "\n" ++ replicate 50 '='
    putStrLn $ "CURRENCY: " ++ currencyName analysis ++ " (" ++ currencyCode analysis ++ ")"
    putStrLn $ replicate 50 '='
    
    putStrLn "\n📊 LATEST RATES (last 5 days):"
    mapM_ printRate $ take 5 $ sortBy (comparing (Data.Ord.Down . effectiveDate)) (rates ex)
    
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

exportToCSV :: [CurrencyAnalysis] -> IO ()
exportToCSV analyses = do
    let csvHeader = "Currency,Current Rate,Average,Min,Max,Volatility,Change %,Trend\n"
    let csvRows = map analysisToCSV analyses
    writeFile "results/basic_currency_analysis.csv" (csvHeader ++ unlines csvRows)
    putStrLn "✅ Basic analysis exported to results/basic_currency_analysis.csv"

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

exportAdvancedAnalysis :: [(String, [Double])] -> IO ()
exportAdvancedAnalysis currencyData = do
    let correlations = calculateCorrelationMatrix currencyData
    let corrHeader = "Currency1,Currency2,Correlation\n"
    let corrRows = map (\(c1, c2, corr) -> c1 ++ "," ++ c2 ++ "," ++ show corr) correlations
    writeFile "results/correlation_matrix.csv" (corrHeader ++ unlines corrRows)
    
    let trendHeader = "Currency,Predicted_Rate,Trend_Strength,VaR_95%,Max_Drawdown\n"
    let trendRows = map formatTrendData currencyData
    writeFile "results/trend_analysis.csv" (trendHeader ++ unlines trendRows)
    
    let fullReport = generateAdvancedReport currencyData
    writeFile "results/advanced_analysis_report.txt" (unlines fullReport)
    
    putStrLn "✅ Advanced analysis exported to results/ directory"

formatTrendData :: (String, [Double]) -> String
formatTrendData (currCode, rateList) =
    let prediction = predictNextValue rateList
        strength = trendStrength rateList
        var95 = calculateVaR 0.95 rateList
        maxDD = maxDrawdown rateList
    in currCode ++ "," ++ 
       show prediction ++ "," ++ 
       show strength ++ "," ++ 
       show var95 ++ "," ++ 
       show maxDD

fetchDateRatesForCharts :: String -> IO (String, [(String, Double)])
fetchDateRatesForCharts curr = do
    let url = "https://api.nbp.pl/api/exchangerates/rates/A/" ++ curr ++ "/last/" ++ show numOfLastCurr ++ "/?format=json"
    request <- parseRequest url
    response <- httpLBS request
    let body = getResponseBody response
    case decode body :: Maybe ExchangeRates of
        Just ex -> do
            let dateRatePairs = [(effectiveDate rate, mid rate) | rate <- reverse (rates ex)]
            return (curr, dateRatePairs)
        Nothing -> do
            putStrLn $ "Error parsing date data for charts: " ++ curr
            return (curr, [])

fetchDataForCurrency :: String -> IO ()
fetchDataForCurrency curr = do
    _ <- fetchAndAnalyzeCurrency curr
    return ()