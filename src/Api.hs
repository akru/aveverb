module Api (api, loadDB) where

import Api.Types
import Api.Process

import Happstack.Server ( ServerPart, Response, askRq, takeRequestBody
                        , unBody, toResponse, badRequest, ok)
import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as L 
import qualified Data.Binary as B (decode)
import Data.Aeson

loadDB :: String -> IO Database
loadDB name = do
    dbf <- L.readFile name
    return $ B.decode (decompress dbf)

api :: Database -> ServerPart Response
api db = do
    b <- getBody
    case b of
        Just req ->
            ok $ toResponse $ process db req
        Nothing  ->
            badRequest $ toResponse "Bad request."
  where
    getBody = askRq >>= takeRequestBody >>= return . json
    json v  = decode =<< fmap unBody v

