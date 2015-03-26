{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
module Api (api) where

import Config

import Prelude hiding (lookup)
import Happstack.Server (ServerPart, Response, toResponse, badRequest, ok)
import Happstack.Server.Routing (uriRest)
import Control.Monad.IO.Class (liftIO)
import Data.List.Split (splitOn)
import Data.Aeson (encode)
import Database.MongoDB

api :: Pipe -> ServerPart Response
api pipe = uriRest $ \uri ->
    case uri of
        [] -> badRequest $ toResponse "Too few arguments"
        _  -> do
            res <- liftIO $ processMethod (toReq uri)
            ok $ toResponse res
  where
    toReq = splitOn "/" . tail
    runDB = access pipe ReadStaleOk dbname

    selectWith :: Val v => Query -> Label -> IO [v]
    selectWith q l = do
        r <- runDB (rest =<< find q)
        mapM (lookup l) r

    processMethod ("list" : _) = do
        names <- selectWith (select [] verb_c) "name"
        return $ encode (names :: [String])

    processMethod ("rule" : verb : _) = do
        rules <- selectWith (select ["name" =: verb] verb_c) "rule"
        return $ encode (head rules :: String)
    
    processMethod ("forms" : verb : _) = do
        forms <- selectWith (select ["name" =: verb] verb_c) "forms"
        return $ encode (head forms :: [String])

    processMethod ("samples" : verb : _) = do
        samples <- selectWith (select ["verb" =: verb] samp_c) "samples"
        return $ encode (head samples :: [String])

    processMethod _ = error $ "Unknown request!"

