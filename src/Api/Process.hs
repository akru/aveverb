{-# LANGUAGE OverloadedStrings #-}
module Api.Process (process) where

import Api.Types

import Data.Maybe (catMaybes, fromMaybe)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.UTF8 (toString)
import Control.Applicative ((<$>))
import qualified Data.Map as M
import Data.Map (Map)
import Data.Aeson

process :: Database -> [String] -> ByteString
process db r =
    processMethod (head r)
  where
    processMethod "list" =
        encode $ verbList db

    processMethod "samples" =
        encode $ loadSamples db (tail r)

    processMethod m =
        encode ("Unknown method `" ++ concat r ++ "`!" :: String)

loadSamples :: Database -> [String] -> [Map String [String]]
loadSamples db =
    map (M.fromList . map (\a -> (a, samplesBy a))) . formsFor
  where
    lemmes    = lemmeMap db
    samples   = sampleMap db
    formsFor  = catMaybes . map (flip M.lookup lemmes)
    samplesBy = map toString . fromMaybe [] . flip M.lookup samples

