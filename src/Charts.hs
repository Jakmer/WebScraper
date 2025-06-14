{-# LANGUAGE OverloadedStrings #-}

module Charts where

import Data.List (intercalate)
import System.Directory (createDirectoryIfMissing)
import Analytics

-- Konfiguracja wykresów
chartWidth, chartHeight :: Double
chartWidth = 900
chartHeight = 700

margin :: Double
margin = 80

-- Główna funkcja generująca wszystkie wykresy
generateAllCharts :: [(String, [Double])] -> [(String, [(String, Double)])] -> IO ()
generateAllCharts currencyData _ = do
    createDirectoryIfMissing True "results/charts"
    putStrLn "📊 Generating SVG charts..."
    
    -- Wykresy kursów dla każdej waluty (pierwsze 4)
    mapM_ generatePriceChart (take 4 currencyData)
    
    -- Wykres porównania volatility
    generateVolatilityChart currencyData
    
    -- Wykres porównania aktualnych kursów
    generateCurrentRatesChart currencyData
    
    -- Wykres korelacji
    generateCorrelationChart currencyData
    
    putStrLn "✅ SVG charts saved to results/charts/ directory"

-- Wykres kursu w czasie dla pojedynczej waluty
generatePriceChart :: (String, [Double]) -> IO ()
generatePriceChart (currency, rates) = do
    let fileName = "results/charts/" ++ currency ++ "_price_chart.svg"
        points = zipWith (\i rate -> (fromIntegral (i :: Int), rate)) [0..] rates
        svg = generateLineChart currency "Days" "Rate (PLN)" points
    writeFile fileName svg

-- Wykres porównania volatility
generateVolatilityChart :: [(String, [Double])] -> IO ()
generateVolatilityChart currencyData = do
    let volatilities = map (\(curr, rates) -> (curr, standardDeviation rates)) currencyData
        fileName = "results/charts/volatility_comparison.svg"
        svg = generateBarChart "Currency Volatility Comparison" "Currency" "Standard Deviation (PLN)" volatilities
    writeFile fileName svg

-- Wykres aktualnych kursów
generateCurrentRatesChart :: [(String, [Double])] -> IO ()
generateCurrentRatesChart currencyData = do
    let currentRates = map (\(curr, rates) -> (curr, if null rates then 0 else head rates)) currencyData
        fileName = "results/charts/current_rates.svg"
        svg = generateBarChart "Current Exchange Rates" "Currency" "Rate (PLN)" currentRates
    writeFile fileName svg

-- Wykres korelacji
generateCorrelationChart :: [(String, [Double])] -> IO ()
generateCorrelationChart currencyData = do
    let correlations = calculateCorrelationMatrix currencyData
        strongCorrelations = filter (\(_, _, corr) -> abs corr > 0.5) correlations
        corrData = map (\(c1, c2, corr) -> (c1 ++ "-" ++ c2, corr)) strongCorrelations
        fileName = "results/charts/correlations.svg"
    
    if null strongCorrelations
        then putStrLn "⚠️  No strong correlations found (|r| > 0.5)"
        else do
            let svg = generateBarChart "Strong Currency Correlations (|r| > 0.5)" "Currency Pairs" "Correlation" corrData
            writeFile fileName svg

-- Generator wykresu liniowego SVG
generateLineChart :: String -> String -> String -> [(Double, Double)] -> String
generateLineChart title xLabel yLabel points = 
    let plotWidth = chartWidth - 2 * margin
        plotHeight = chartHeight - 2 * margin
        
        (minX, maxX) = if null points then (0, 1) else (minimum (map fst points), maximum (map fst points))
        (minY, maxY) = if null points then (0, 1) else (minimum (map snd points), maximum (map snd points))
        
        rangeX = if maxX == minX then 1 else maxX - minX
        rangeY = if maxY == minY then 1 else maxY - minY
        
        scaleX x = margin + (x - minX) / rangeX * plotWidth
        scaleY y = chartHeight - margin - (y - minY) / rangeY * plotHeight
        
        pathData = intercalate " " $ zipWith (\i (x, y) -> 
            (if (i :: Int) == 0 then "M " else "L ") ++ show (scaleX x) ++ "," ++ show (scaleY y)) [0..] points
        
        gridLines = generateYAxisGrid minY maxY scaleY
        yAxisLabels = generateYAxisLabels minY maxY scaleY
        xAxisLabels = generateXAxisLabels minX maxX scaleX (length points)
            
    in svgHeader ++ 
       gridLines ++
       "<path d=\"" ++ pathData ++ "\" fill=\"none\" stroke=\"#2196F3\" stroke-width=\"2\"/>\n" ++
       yAxisLabels ++
       xAxisLabels ++
       "<text x=\"" ++ show (chartWidth / 2) ++ "\" y=\"30\" text-anchor=\"middle\" font-family=\"Arial\" font-size=\"18\" font-weight=\"bold\">" ++ title ++ "</text>\n" ++
       "<text x=\"" ++ show (chartWidth / 2) ++ "\" y=\"" ++ show (chartHeight - 10) ++ "\" text-anchor=\"middle\" font-family=\"Arial\" font-size=\"14\">" ++ xLabel ++ "</text>\n" ++
       "<text x=\"25\" y=\"" ++ show (chartHeight / 2) ++ "\" text-anchor=\"middle\" font-family=\"Arial\" font-size=\"14\" transform=\"rotate(-90 25 " ++ show (chartHeight / 2) ++ ")\">" ++ yLabel ++ "</text>\n" ++
       svgFooter

-- Generator wykresu słupkowego SVG
generateBarChart :: String -> String -> String -> [(String, Double)] -> String
generateBarChart title xLabel yLabel dataPoints =
    let plotWidth = chartWidth - 2 * margin
        plotHeight = chartHeight - 2 * margin
        
        values = map snd dataPoints
        
        minY = if null values then 0 else min 0 (minimum values)
        maxY = if null values then 1 else maximum values
        rangeY = if maxY == minY then 1 else maxY - minY
        
        barWidth = plotWidth / fromIntegral (max 1 (length dataPoints))
        
        scaleY y = chartHeight - margin - (y - minY) / rangeY * plotHeight
        zeroY = scaleY 0
        
        bars = concatMap (\(i, (_, value)) ->
            let x = margin + fromIntegral (i :: Int) * barWidth + barWidth * 0.1
                barHeight = abs (scaleY value - zeroY)
                y = min (scaleY value) zeroY
                color = if value >= 0 then "#4CAF50" else "#F44336"
            in "<rect x=\"" ++ show x ++ "\" y=\"" ++ show y ++ "\" width=\"" ++ show (barWidth * 0.8) ++ "\" height=\"" ++ show barHeight ++ "\" fill=\"" ++ color ++ "\"/>\n"
            ) (zip [(0 :: Int)..] dataPoints)
        
        labelTexts = concatMap (\(i, (label, _)) ->
            let x = margin + fromIntegral (i :: Int) * barWidth + barWidth * 0.5
                y = chartHeight - margin + 20
                displayLabel = if length label > 8 then take 6 label ++ ".." else label
            in "<text x=\"" ++ show x ++ "\" y=\"" ++ show y ++ "\" text-anchor=\"middle\" font-family=\"Arial\" font-size=\"11\">" ++ displayLabel ++ "</text>\n"
            ) (zip [(0 :: Int)..] dataPoints)
        
        gridLines = generateYAxisGrid minY maxY scaleY
        yAxisLabels = generateYAxisLabels minY maxY scaleY
        
    in svgHeader ++ 
       gridLines ++
       bars ++
       labelTexts ++
       yAxisLabels ++
       "<text x=\"" ++ show (chartWidth / 2) ++ "\" y=\"30\" text-anchor=\"middle\" font-family=\"Arial\" font-size=\"18\" font-weight=\"bold\">" ++ title ++ "</text>\n" ++
       "<text x=\"" ++ show (chartWidth / 2) ++ "\" y=\"" ++ show (chartHeight - 5) ++ "\" text-anchor=\"middle\" font-family=\"Arial\" font-size=\"14\">" ++ xLabel ++ "</text>\n" ++
       "<text x=\"25\" y=\"" ++ show (chartHeight / 2) ++ "\" text-anchor=\"middle\" font-family=\"Arial\" font-size=\"14\" transform=\"rotate(-90 25 " ++ show (chartHeight / 2) ++ ")\">" ++ yLabel ++ "</text>\n" ++
       svgFooter

-- Funkcje pomocnicze do generowania osi i siatki
generateYAxisGrid :: Double -> Double -> (Double -> Double) -> String
generateYAxisGrid minY maxY scaleY =
    let numLines = (6 :: Int)
        step = (maxY - minY) / fromIntegral (numLines - 1)
        yValues = [minY + fromIntegral i * step | i <- [0..numLines-1]]
        gridLines = concatMap (generateGridLine scaleY) yValues
    in gridLines

generateGridLine :: (Double -> Double) -> Double -> String
generateGridLine scaleY y =
    let yPos = scaleY y
    in "<line x1=\"" ++ show margin ++ "\" y1=\"" ++ show yPos ++ "\" x2=\"" ++ show (chartWidth - margin) ++ "\" y2=\"" ++ show yPos ++ "\" stroke=\"#ddd\" stroke-width=\"1\"/>\n"

generateYAxisLabels :: Double -> Double -> (Double -> Double) -> String
generateYAxisLabels minY maxY scaleY =
    let numLabels = (6 :: Int)
        step = (maxY - minY) / fromIntegral (numLabels - 1)
        yValues = [minY + fromIntegral i * step | i <- [0..numLabels-1]]
        labels = concatMap (generateYLabel scaleY) yValues
    in labels

generateYLabel :: (Double -> Double) -> Double -> String
generateYLabel scaleY y =
    let yPos = scaleY y
        labelText = formatYValue y
    in "<text x=\"" ++ show (margin - 10) ++ "\" y=\"" ++ show (yPos + 5) ++ "\" text-anchor=\"end\" font-family=\"Arial\" font-size=\"11\">" ++ labelText ++ "</text>\n"

formatYValue :: Double -> String
formatYValue y 
    | abs y < 0.01 = "0"
    | abs y >= 1000 = show (round y :: Int)
    | abs y >= 1 = formatDecimal y 2
    | otherwise = formatDecimal y 4

generateXAxisLabels :: Double -> Double -> (Double -> Double) -> Int -> String
generateXAxisLabels minX maxX scaleX numPoints =
    let numLabels = min 8 numPoints
        step = if numLabels <= 1 then 0 else (maxX - minX) / fromIntegral (numLabels - 1)
        xValues = [minX + fromIntegral i * step | i <- [0..numLabels-1]]
        labels = concatMap (generateXLabel scaleX) xValues
    in labels

generateXLabel :: (Double -> Double) -> Double -> String
generateXLabel scaleX x =
    let xPos = scaleX x
        labelText = show (round x :: Int)
    in "<text x=\"" ++ show xPos ++ "\" y=\"" ++ show (chartHeight - margin + 20) ++ "\" text-anchor=\"middle\" font-family=\"Arial\" font-size=\"11\">" ++ labelText ++ "</text>\n"

formatDecimal :: Double -> Int -> String
formatDecimal x precision = 
    let factor = 10 ^ precision
        rounded = (fromIntegral (round (x * factor) :: Int)) / factor
    in if rounded == (fromIntegral (round rounded :: Int))
       then show (round rounded :: Int)
       else take (precision + 3) (show rounded)

-- Szablony SVG
svgHeader :: String
svgHeader = 
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
    "<svg width=\"" ++ show (round chartWidth :: Int) ++ "\" height=\"" ++ show (round chartHeight :: Int) ++ "\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
    "<rect width=\"100%\" height=\"100%\" fill=\"white\"/>\n"

svgFooter :: String
svgFooter = "</svg>\n"