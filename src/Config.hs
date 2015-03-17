{-# LANGUAGE OverloadedStrings #-}
module Config where

import Database.MongoDB

dbname :: Database
dbname = "aveverb"

dbhost :: Host
dbhost = host "127.0.0.1"


verb_c, samp_c :: Collection
verb_c = "verbs"
samp_c = "samples"

