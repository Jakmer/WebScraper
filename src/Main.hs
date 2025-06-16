module Main where

import DataFetcher (fetchData)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    
    putStrLn "╔════════════════════════════════════════════════════════════════╗"
    putStrLn "║                    CURRENCY ANALYZER 2025                     ║"
    putStrLn "║                Advanced Functional Analytics                  ║"
    putStrLn "╚════════════════════════════════════════════════════════════════╝"
    putStrLn ""
    
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Starting comprehensive currency analysis..."
            fetchData
        ["--help"] -> printHelp
        ["--version"] -> putStrLn "Currency Analyzer v1.0"
        _ -> do
            putStrLn "Unknown arguments. Use --help for usage information."
            fetchData

printHelp :: IO ()
printHelp = do
    putStrLn "Usage: cabal run WebScraper [OPTION]"
    putStrLn ""
    putStrLn "Options:"
    putStrLn "  (no args)   Run full currency analysis"
    putStrLn "  --help      Show this help message"
    putStrLn "  --version   Show version information"
    putStrLn ""
    putStrLn "Output files will be created in the 'results/' directory:"
    putStrLn "  - basic_currency_analysis.csv     (Basic statistical analysis)"
    putStrLn "  - correlation_matrix.csv          (Currency correlations)"
    putStrLn "  - trend_analysis.csv              (Predictions and risk metrics)"
    putStrLn "  - advanced_analysis_report.txt    (Comprehensive text report)"