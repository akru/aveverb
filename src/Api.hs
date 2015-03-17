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

    processMethod ("list" : _) = do
        let verbNames = (select [] verb_c) { project = ["name" =: 1] }
        r <- runDB $ rest =<< find verbNames
        l <- mapM (lookup "name") r
        return $ encode (l :: [String])

    processMethod ("rule" : verb : _) = do
        let verbRules = (select ["name" =: verb] verb_c) { project = ["rule" =: 1] }
        r <- runDB $ rest =<< find verbRules
        rule <- lookup "rule" (head r)
        return $ encode (rule :: String)
    
    processMethod ("forms" : verb : _) = do
        let verbForms = (select ["name" =: verb] verb_c) { project = ["forms" =: 1] }
        r <- runDB $ rest =<< find verbForms
        s <- lookup "forms" (head r)
        return $ encode (s :: [String])

    processMethod ("samples" : verb : _) = do
        let samples = (select ["verb" =: verb] samp_c) { project = ["samples" =: 1] }
        r <- runDB $ rest =<< find samples
        s <- lookup "samples" (head r)
        return $ encode (s :: [String])

    processMethod m
        = error $ "Unknown request `" ++ show m ++ "`!"

