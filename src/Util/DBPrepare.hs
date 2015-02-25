{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Control.Applicative ((<$>))
import qualified Data.Map as M
import Codec.Compression.GZip
import Data.Binary

import Api.Types
import Api.LemmeParser

main :: IO ()
main = do
    putStrLn "Resource loading..."
    verbs <- lines <$> readFile "data/verbs.txt"
    putStrLn $ "Verbs count..." ++ show (length verbs)
    lemmes <- loadLemmes "data/lemmes.txt"
    putStrLn $ "Lemmes count..." ++ show (M.size lemmes)
    samples <- (decode . decompress) <$> L.readFile "data/samples.gz"
    putStrLn $ "Samples count..." ++ show (M.size samples)
    putStrLn "Done"
    L.writeFile "db.gz" $ compress $ encode (Database verbs lemmes samples)
    putStrLn "Database `db.gz` saved!"

