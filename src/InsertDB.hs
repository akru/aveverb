{-# LANGUAGE OverloadedStrings #-}
module Main where

import Config

import Prelude hiding (lookup)
import Database.MongoDB
import System.Environment
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as L
import Control.Monad (forM)
import Text.Regex.Posix
import Debug.Trace

findSamples samples v = do
    verb <- lookup "name" v
    forms <- lookup "forms" v
    trace (show verb) (return ())
    return $
        [ "verb"    =: verb
        , "samples" =: map L.toString (filter (=~ pattern (verb : forms)) samples)
        ]
  where
    pattern :: [String] -> L.ByteString
    pattern forms = L.fromString $ "(" ++ tail (concat (map (\a -> "| " ++ a ++ " ") forms)) ++ ")"

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

