{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (msum)
import Happstack.Server (simpleHTTP, nullConf, dir)
import Happstack.Server.FileServe
import Database.MongoDB.Connection

import Config (dbhost)
import Index (index)
import Api (api)

main :: IO ()
main = do
    pipe <- connect dbhost
    simpleHTTP nullConf $ msum 
        [ dir "api"    $ api pipe
        , dir "static" $ serveDirectory DisableBrowsing [] "static"
        , index
        ]

