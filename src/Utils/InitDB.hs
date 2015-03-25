{-# LANGUAGE OverloadedStrings #-}
module Main where

import Config
import Utils.LemmeParser

import qualified Data.Map as M
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Database.MongoDB

main :: IO ()
main = do
    pipe <- connect dbhost
    args <- getArgs
    
    f <- readFile (head args)
    let verbRules = lines f

    lemmes <- loadLemmes (args !! 1)

    access pipe master dbname $ do
        delete (select [] verb_c)
        insertMany verb_c (fields lemmes verbRules)
    putStrLn "Done"
  where
    fields l = map (\v ->
        let verb = head (words v) in 
            [ "name"  =: verb
            , "rule"  =: v
            , "forms" =: fromMaybe [] (M.lookup verb l)
            ])

