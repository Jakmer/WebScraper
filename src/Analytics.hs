{-# LANGUAGE OverloadedStrings #-}

module Analytics where

import Data.List (sortBy, tails, transpose, groupBy)
import Data.Ord (comparing)
import Data.Function (on)
import Text.Printf
import Control.Monad.Writer
import Data.Time
import Data.Time.Format

-- Średnia ruchoma
simpleMovingAverage :: Int -> [Double] -> [Double]
simpleMovingAverage n rates
    | n <= 0 = []
    | otherwise = map average $ takeWhile ((>= n) . length) $ map (take n) $ tails rates
  where
    average xs = sum xs / fromIntegral (length xs)

-- Odchylenie standardowe
standardDeviation :: [Double] -> Double
standardDeviation [] = 0
standardDeviation xs = sqrt $ sum (map (\x -> (x - avg) ** 2) xs) / fromIntegral (length xs)
  where avg = sum xs / fromIntegral (length xs)

-- RSI (Relative Strength Index)
calculateRSI :: Int -> [Double] -> [Double]
calculateRSI period rates
    | length rates < period + 1 = []
    | otherwise = 
        let changes = zipWith (-) (tail rates) rates
            gains = map (max 0) changes
            losses = map (abs . min 0) changes
            avgGains = simpleMovingAverage period gains
            avgLosses = simpleMovingAverage period losses
        in zipWith (\g l -> if l == 0 then 100 else 100 - (100 / (1 + g/l))) avgGains avgLosses

-- Korelacja Pearsona
pearsonCorrelation :: [Double] -> [Double] -> Double
pearsonCorrelation xs ys
    | length xs /= length ys = 0
    | length xs < 2 = 0
    | otherwise = 
        let n = fromIntegral $ length xs
            sumX = sum xs
            sumY = sum ys
            sumXY = sum $ zipWith (*) xs ys
            sumX2 = sum $ map (** 2) xs
            sumY2 = sum $ map (** 2) ys
            numerator = n * sumXY - sumX * sumY
            denominator = sqrt ((n * sumX2 - sumX ** 2) * (n * sumY2 - sumY ** 2))
        in if denominator == 0 then 0 else numerator / denominator

-- Macierz korelacji
calculateCorrelationMatrix :: [(String, [Double])] -> [(String, String, Double)]
calculateCorrelationMatrix currencyRates =
    [ (name1, name2, pearsonCorrelation rates1 rates2)
    | (name1, rates1) <- currencyRates
    , (name2, rates2) <- currencyRates
    , name1 < name2
    ]

-- Regresja liniowa
linearRegression :: [Double] -> (Double, Double)
linearRegression ys =
    let xs = map fromIntegral [1..length ys]
        n = fromIntegral $ length xs
        sumX = sum xs
        sumY = sum ys
        sumXY = sum $ zipWith (*) xs ys
        sumX2 = sum $ map (** 2) xs
        denominator = n * sumX2 - sumX ** 2
        slope = if denominator == 0 then 0 else (n * sumXY - sumX * sumY) / denominator
        intercept = (sumY - slope * sumX) / n
    in (slope, intercept)

-- Predykcja następnej wartości
predictNextValue :: [Double] -> Double
predictNextValue rates =
    let (slope, intercept) = linearRegression rates
        nextX = fromIntegral $ length rates + 1
    in slope * nextX + intercept

-- Siła trendu (R-squared)
trendStrength :: [Double] -> Double
trendStrength rates =
    let (slope, intercept) = linearRegression rates
        xs = map fromIntegral [1..length rates]
        predicted = map (\x -> slope * x + intercept) xs
        meanY = sum rates / fromIntegral (length rates)
        totalSumSquares = sum $ map (\y -> (y - meanY) ** 2) rates
        residualSumSquares = sum $ zipWith (\actual pred -> (actual - pred) ** 2) rates predicted
    in if totalSumSquares == 0 then 0 else 1 - (residualSumSquares / totalSumSquares)

-- Value at Risk (VaR)
calculateVaR :: Double -> [Double] -> Double
calculateVaR confidenceLevel rates =
    let changes = zipWith (\current prev -> (current - prev) / prev) (tail rates) rates
        sortedChanges = sortBy compare changes
        index = floor $ (1 - confidenceLevel) * fromIntegral (length sortedChanges)
        safeIndex = max 0 $ min (length sortedChanges - 1) index
    in if null sortedChanges then 0 else sortedChanges !! safeIndex

-- Maksymalne spadki
maxDrawdown :: [Double] -> Double
maxDrawdown rates =
    let cumulativeMax = scanl1 max rates
        drawdowns = zipWith (\current maxVal -> (current - maxVal) / maxVal) rates cumulativeMax
    in if null drawdowns then 0 else minimum drawdowns

-- Analiza według dni tygodnia
analyzeDayOfWeek :: [(String, Double)] -> [(String, Double)]
analyzeDayOfWeek dateRatePairs =
    let parseDate dateStr = parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr :: Maybe Day
        dayOfWeekData = [(show (dayOfWeek day), rate) | (dateStr, rate) <- dateRatePairs, 
                                                       Just day <- [parseDate dateStr]]
        groupedByDay = groupBy ((==) `on` fst) $ sortBy (comparing fst) dayOfWeekData
        avgByDay = map (\group -> (fst $ head group, average $ map snd group)) groupedByDay
    in avgByDay
  where
    average xs = sum xs / fromIntegral (length xs)

type AnalysisReport = Writer [String] ()

generateAdvancedReport :: [(String, [Double])] -> [String]
generateAdvancedReport currencyData = execWriter $ do
    tell ["", "╔════════════════════════════════════════════════════════════════╗"]
    tell ["║                    ADVANCED CURRENCY ANALYSIS                 ║"]
    tell ["╚════════════════════════════════════════════════════════════════╝"]
    
    correlationReport currencyData
    trendReport currencyData
    riskReport currencyData
    technicalReport currencyData

correlationReport :: [(String, [Double])] -> AnalysisReport
correlationReport currencyData = do
    tell ["", "📊 CORRELATION ANALYSIS:"]
    tell ["    (Values close to 1: strong positive correlation)"]
    tell ["    (Values close to -1: strong negative correlation)"]
    tell ["    (Values close to 0: no correlation)", ""]
    
    let correlations = calculateCorrelationMatrix currencyData
        strongCorrelations = filter (\(_, _, corr) -> abs corr > 0.7) correlations
    
    mapM_ (\(c1, c2, corr) -> 
        tell [printf "    %s ↔ %s: %+.3f" c1 c2 corr]) correlations
    
    whenCondition (not $ null strongCorrelations) $ do
        tell ["", "    🔍 STRONG CORRELATIONS (|r| > 0.7):"]
        mapM_ (\(c1, c2, corr) -> 
            tell [printf "    🔗 %s ↔ %s: %+.3f" c1 c2 corr]) strongCorrelations

trendReport :: [(String, [Double])] -> AnalysisReport
trendReport currencyData = do
    tell ["", "📈 TREND ANALYSIS:"]
    tell ["    (R² close to 1: strong trend, close to 0: no clear trend)", ""]
    
    mapM_ (\(currency, rates) -> do
        let (slope, _) = linearRegression rates
            strength = trendStrength rates
            prediction = predictNextValue rates
            trendDirection :: String
            trendDirection = if slope > 0 then "📈 Rising" else "📉 Falling"
        tell [printf "    %s:" currency]
        tell [printf "      Trend: %s (R² = %.3f)" trendDirection strength]
        tell [printf "      Predicted next rate: %.4f PLN" prediction]
        tell [""]) currencyData

riskReport :: [(String, [Double])] -> AnalysisReport
riskReport currencyData = do
    tell ["", "⚠️  RISK ANALYSIS:"]
    tell ["    (VaR 95%: maximum expected loss with 95% confidence)", ""]
    
    mapM_ (\(currency, rates) -> do
        let var95 = calculateVaR 0.95 rates
            maxDD = maxDrawdown rates
            volatility = standardDeviation rates
        tell [printf "    %s:" currency]
        tell [printf "      VaR (95%%): %.2f%%" (var95 * 100)]
        tell [printf "      Max Drawdown: %.2f%%" (maxDD * 100)]
        tell [printf "      Volatility: %.4f PLN" volatility]
        tell [""]) currencyData

technicalReport :: [(String, [Double])] -> AnalysisReport
technicalReport currencyData = do
    tell ["", "🔧 TECHNICAL INDICATORS:"]
    tell ["    (RSI > 70: potentially overbought, RSI < 30: potentially oversold)", ""]
    
    mapM_ (\(currency, rates) -> do
        let sma20 = take 5 $ simpleMovingAverage 20 rates
            rsi = take 5 $ calculateRSI 14 rates
            currentRate = if null rates then 0 else head rates
            currentSMA = if null sma20 then 0 else head sma20
            currentRSI = if null rsi then 50 else head rsi
        
        tell [printf "    %s:" currency]
        tell [printf "      Current Rate: %.4f PLN" currentRate]
        tell [printf "      SMA(20): %.4f PLN" currentSMA]
        tell [printf "      RSI(14): %.1f" currentRSI]
        
        let rsiSignal :: String
            rsiSignal | currentRSI > 70 = "🔴 Overbought"
                      | currentRSI < 30 = "🟢 Oversold"
                      | otherwise = "🟡 Neutral"
        tell [printf "      RSI Signal: %s" rsiSignal]
        tell [""]) currencyData

whenCondition :: Monad m => Bool -> m () -> m ()
whenCondition True action = action
whenCondition False _ = return ()