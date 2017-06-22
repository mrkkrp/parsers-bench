module ParsersBench.CSV.Megaparsec
  ( parseCSV )
where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Vector           as V

type Parser = Parsec Dec ByteString
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
  V.fromList <$!> (sepBy1 field (blindByte ',') <?> "record")

field :: Parser Field
field = label "field" (escapedField <|> unescapedField)

escapedField :: Parser ByteString
escapedField =
  BC8.pack <$!> between (char '"') (char '"') (many $ normalChar <|> escapedDq)
  where
    normalChar = noneOf "\"" <?> "unescaped character"
    escapedDq  = label "escaped double-quote" ('"' <$ string "\"\"")

unescapedField :: Parser ByteString
unescapedField = BC8.pack <$!> many (noneOf ",\"\n\r")
{-# INLINE unescapedField #-}

blindByte :: Char -> Parser ()
blindByte = void . char
{-# INLINE blindByte #-}
