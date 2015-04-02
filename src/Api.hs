{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Api (api) where

import Config

import Prelude hiding (lookup)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.UTF8 (toString, ByteString)
import Data.Aeson (encode)
import Database.MongoDB
import Data.Map ((!))
import Snap.Core

data REST = REST ByteString Params
  deriving Show

selectWith :: Val v => Query -> Label -> Snap [v]
selectWith q l = liftIO $ do
    pipe <- connect dbhost
    let runDB = access pipe ReadStaleOk dbname
    r <- runDB (rest =<< find q {project = [l =: 1]})
    mapM (lookup l) r

api :: Snap ()
api = do
    r <- getRequest
    process $ REST (rqPathInfo r) (rqQueryParams r)
  where
    process (REST "list" args) = do
        let listName = argOne (args ! "name")
            allVerbs = selectWith (select [] verb_c) "name"
        names <- case listName of
            "all"     -> allVerbs
            "popular" -> allVerbs
            _         -> allVerbs
        writeLBS $ encode (names :: [String])

    process (REST "rule" args) = do
        let verb = argOne (args ! "verb")
            ruleByVerb = select ["name" =: verb] verb_c
        rules <- selectWith ruleByVerb "rule"
        writeLBS $ encode (head rules :: String)
    
    process (REST "samples" args) = do
        let verb = argOne (args ! "verb")
            page = read $ argOne $ args ! "page"
            cnt  = read $ argOne $ args ! "count"
            samplesByVerb = (select ["verb" =: verb] samp_c)
        samples <- selectWith samplesByVerb "samples"
        let paginated = take cnt . drop (page * cnt) . head
        writeLBS $ encode (paginated samples :: [String])

    process _ = error $ "Unknown request method!"
    argOne = toString . head

