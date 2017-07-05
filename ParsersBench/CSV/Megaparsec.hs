{-# LANGUAGE OverloadedStrings #-}

module ParsersBench.CSV.Megaparsec
  ( parseCSV )
where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Data.Void
import Data.Word (Word8)
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Data.ByteString as B
import qualified Data.Vector     as V

type Parser = Parsec Void ByteString
type Record = Vector Field
type Field  = ByteString

-- | Parse a CSV file without conversion of individual records.

parseCSV :: ByteString -> [Record]
parseCSV bs =
  case parse csv "" bs of
    Left err -> error (parseErrorPretty err)
    Right x -> x

csv :: Parser [Record]
csv = do
  xs <- sepEndBy1 record eol
  eof
  return xs

record :: Parser Record
record = do
  notFollowedBy eof -- to prevent reading empty line at the end of file
  V.fromList <$!> (sepBy1 field (blindByte 44) <?> "record")

field :: Parser Field
field = label "field" (escapedField <|> unescapedField)

escapedField :: Parser ByteString
escapedField =
  B.pack <$!> between (char 34) (char 34) (many $ normalChar <|> escapedDq)
  where
    normalChar = notChar 34 <?> "unescaped character"
    escapedDq  = label "escaped double-quote" (34 <$ string "\"\"")

unescapedField :: Parser ByteString
unescapedField = takeWhileP
  (Just "unescaped char")
  (`notElem` [44,34,10,13])
{-# INLINE unescapedField #-}

blindByte :: Word8 -> Parser ()
blindByte = void . char
{-# INLINE blindByte #-}
