{-# LANGUAGE OverloadedStrings #-}

module ParsersBench.CSV.Attoparsec
  ( parseCSV )
where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as BC8
import qualified Data.Vector                      as V

type Record = Vector Field
type Field  = ByteString

parseCSV :: ByteString -> [Record]
parseCSV bs =
  case parseOnly csv bs of
    Left err -> error err
    Right x -> x

csv :: Parser [Record]
csv = do
  xs <- sepBy1 record endOfLine
  option () endOfLine -- attoparsec doesn't have sepEndBy1
  endOfInput
  return xs

record :: Parser Record
record = do
  endAlready <- atEnd
  when endAlready empty -- to prevent reading empty line at the end of file
  V.fromList <$!> (sepBy1 field (blindByte ',') <?> "record")

field :: Parser Field
field = (escapedField <|> unescapedField) <?> "field"

escapedField :: Parser ByteString
escapedField = do
  void (char '"')
  let normalChar = notChar '\"' <?> "unescaped character"
      escapedDq  = '"' <$ string "\"\""
  xs <- BC8.pack <$!> many (normalChar <|> escapedDq)
  void (char '"')
  return xs

unescapedField :: Parser ByteString
unescapedField = -- BC8.pack <$!> many (noneOf ",\"\n\r")
  A.takeWhile (`notElem` (",\"\n\r" :: String))
{-# INLINE unescapedField #-}

blindByte :: Char -> Parser ()
blindByte = void . char
{-# INLINE blindByte #-}
