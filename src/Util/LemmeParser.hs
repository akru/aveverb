module Util.LemmeParser (
    Lemme, Form, loadLemmes
) where

import Data.Map.Strict (Map(..), fromList, toList)
import Text.ParserCombinators.Parsec
import System.IO
import Data.Either

type Lemme = String
type Form  = String

parseLemmes :: String -> Map Lemme [Form]
parseLemmes = fromList . rights . map lineParser . lines
  where
    lineParser = parse line "Fran lemmes"

line :: GenParser Char st (Lemme, [Form])
line = do
    w <- many (noneOf "\t")
    char '\t'
    l <- lemmes
    return $ (w, l)

lemmes :: GenParser Char st [String]
lemmes = do
    first <- many (noneOf ";\t")
    next <- remainingLemmes
    return (first : next)

remainingLemmes :: GenParser Char st [String]
remainingLemmes =
    (char ';' >> lemmes) <|> (return [])

readFile' e name = do
    h <- openFile name ReadMode
    enc <- mkTextEncoding e
    hSetEncoding h enc
    hGetContents h

loadLemmes :: FilePath -> IO (Map Lemme [Form]) 
loadLemmes filePath = do
    f <- readFile' "latin1" filePath
    return $ parseLemmes f

