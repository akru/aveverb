{-# LANGUAGE OverloadedStrings #-}
module Main where

import Config

import Prelude hiding (lookup)
import qualified Data.ByteString.Lazy.UTF8 as L
import qualified Data.ByteString.Lazy as L
import System.Environment (getArgs)
import Data.Maybe (catMaybes)
import Control.Monad (forM)
import Text.Regex.Posix
import Database.MongoDB

import Debug.Trace

pattern :: [String] -> L.ByteString
pattern forms =
    L.fromString $ mid "(" ")" patternString
  where
    mid a b c = a ++ c ++ b
    front = "|[' ]"
    back  = "[\\.,;:!\\? ]"
    patternString = tail $ concat $ fmap (mid front back) forms

findSamples samples v = do
    verb  <- lookup "name" v
    forms <- lookup "forms" v
    --trace (show verb) (return ())
    return $
        [ "verb"    =: verb
        , "samples" =: filterAndHL (verb : forms) samples
        ]
  where
    filterAndHL forms samples =
        catMaybes $ fmap (sampleFilter forms) samples

    sampleFilter :: [String] -> String -> Maybe String
    sampleFilter forms sample =
        if sample =~ pattern forms
            then Just $ highlight $ sample =~ pattern forms 
            else Nothing

    highlight :: (String, String, String) -> String
    highlight (a, b, c) = concat [a, "<b>", b, "</b>", c]

main :: IO ()
main = do
    pipe <- connect dbhost
    args <- getArgs
    f <- readFile (head args)
    let samples = lines f
        runDB   = access pipe master dbname
    verbs <- runDB $ rest =<< find (select [] verb_c)

    forM verbs $ \verb -> do
        item <- findSamples samples verb
        runDB $ insert samp_c item
    putStrLn "Done"

