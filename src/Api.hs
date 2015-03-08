module Api (api, loadDB) where

import Api.Types
import Api.Process

import Happstack.Server ( ServerPart, Response, askRq, takeRequestBody
                        , unBody, toResponse, badRequest, ok)
import Happstack.Server.Routing (uriRest)
import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as L 
import qualified Data.Binary as B (decode)
import Data.List.Split (splitOn)
import Data.Aeson

loadDB :: String -> IO Database
loadDB name = do
    dbf <- L.readFile name
    return $ B.decode (decompress dbf)

api :: Database -> ServerPart Response
api db =
    uriRest $ \uri ->
        ok $ toResponse $ process db (splitOn "/" (tail uri))

