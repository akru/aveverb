{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Control.Applicative ((<$>))
import Text.Regex.TDFA
import Api.LemmeParser
import qualified Data.Map as M
import Codec.Compression.GZip
import Data.Binary
import Data.Maybe
import Data.List

main :: IO ()
main = do
    putStrLn "Resource loading..."
    verbs <- lines <$> readFile "data/verbs.txt"
    putStr "Verbs count..."
    putStrLn $ show $ length verbs
    lemmes <- loadLemmes "data/lemmes.txt"
    putStr "Lemmes count..."
    putStrLn $ show $ M.size lemmes
    samples <- B.lines <$> B.readFile "data/news-100K.txt"
    putStr "samples count..."
    putStrLn $ show $ length samples
    putStrLn "Done"
    let formListBy   = catMaybes . map (flip M.lookup lemmes)
        sampleBy a   = (a, filter (=~ a) samples)
        sampleListBy = map (M.fromList . map sampleBy) . formListBy
        result = (M.unions (sampleListBy verbs)) :: M.Map Form [B.ByteString]
     in L.writeFile "samples.gz" $ compress $ encode result

