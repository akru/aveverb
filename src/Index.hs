{-# LANGUAGE OverloadedStrings #-}
module Index where

import Prelude (($))
import qualified Prelude as P

import Happstack.Server (ServerPart, Response, ok, toResponse)
import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

index :: ServerPart Response
index = ok $ toResponse $
    docTypeHtml $ do
        head $ do
            title "AveVerb :: French verbes training"
            script ! A.type_ "text/javascript"
                   ! A.src   "https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"
                   $ ""
            script ! A.type_ "text/javascript"
                   ! A.src   "/static/script.js"
                   $ ""
            link   ! A.rel   "stylesheet"
                   ! A.type_ "text/css"
                   ! A.href  "/static/style.css"
        body $ do
            h1 "Verbes list:"
            ul ""
