{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Http.Server
import Snap.Core
import Api (api)

main :: IO ()
main = quickHttpServe $ dir "api" api

