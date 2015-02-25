{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (msum)
import Happstack.Server (dir, nullConf, notFound, toResponse, simpleHTTP)
import Happstack.Server.FileServe

import Index (index)
import Api (api, loadDB)

main :: IO ()
main = do
    putStrLn "Loading database..."
    db <- loadDB "data/db.gz"
    putStrLn "Done"
    simpleHTTP nullConf $ msum 
        [ dir "api"    $ api db
        , dir "static" $ serveDirectory DisableBrowsing [] "static"
        , index
        ]

