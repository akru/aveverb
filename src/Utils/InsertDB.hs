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
    patternString = tail (concat (map (mid front back) forms))

findSamples samples v = do
    verb <- lookup "name" v
    forms <- lookup "forms" v
    trace (show verb) (return ())
    return $
        [ "verb"    =: verb
        , "samples" =: map L.toString (filterAndHL (verb : forms) samples)
        ]
  where
    filterAndHL forms samples =
        catMaybes $ map (sampleHL forms) samples

    sampleHL forms sample =
        if sample =~ pattern forms
            then let (a, b, c) = sample =~ pattern forms in
                     Just (foldl1 L.append [a, "<b>", b, "</b>", c])
            else Nothing

main :: IO ()
main = do
    pipe <- connect dbhost
    args <- getArgs
    f <- L.readFile (head args)
    let samples = L.lines f
        runDB   = access pipe master dbname
    verbs <- runDB $ rest =<< find (select [] verb_c)

    forM verbs $ \verb -> do
        item <- findSamples samples verb
        runDB $ insert samp_c item
    putStrLn "Done"

