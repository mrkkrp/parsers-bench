module ParsersBench.Json.Megaparsec
  ( parseJson )
where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector (Vector)
import ParsersBench.Json.Common
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.HashMap.Strict   as H
import qualified Data.Text             as T
import qualified Data.Vector           as V
import qualified Text.Megaparsec.Lexer as L

type Parser = Parsec Dec ByteString

parseJson :: ByteString -> Value
parseJson bs =
  case parse json "" bs of
    Left err -> error (parseErrorPretty err)
    Right x -> x

json :: Parser Value
json = json_ object_ array_

json_ :: Parser Value -> Parser Value -> Parser Value
json_ obj ary = do
  w <- skipSpace *> oneOf "{["
  if w == '{'
    then obj
    else ary
{-# INLINE json_ #-}

object_ :: Parser Value
object_ = Object <$> objectValues jstring value

objectValues :: Parser Text -> Parser Value -> Parser (H.HashMap Text Value)
objectValues str val = do
  skipSpace
  let pair = liftA2 (,) (str <* skipSpace) (char ':' *> skipSpace *> val)
  H.fromList <$> commaSeparated pair '}'

array_ :: Parser Value
array_ = Array <$> arrayValues value

arrayValues :: Parser Value -> Parser (Vector Value)
arrayValues val = do
  skipSpace
  V.fromList <$> commaSeparated val ']'

commaSeparated :: Parser a -> Char -> Parser [a]
commaSeparated item endByte = do
  w <- lookAhead anyChar
  if w == endByte
    then [] <$ anyChar
    else loop
  where
    loop = do
      v <- item <* skipSpace
      ch <- oneOf [',', endByte]
      if ch == ','
        then skipSpace >> (v:) <$> loop
        else return [v]
{-# INLINE commaSeparated #-}

value :: Parser Value
value = do
  w <- lookAhead anyChar
  case w of
    '"' -> anyChar *> (String <$> jstring_)
    '{' -> anyChar *> object_
    '[' -> anyChar *> array_
    'f' -> Bool False <$ string "false"
    't' -> Bool True  <$ string "true"
    _
      | w >= '0' && w <= '9' || w == '-' -> Number <$> L.number
      | otherwise -> fail "not a valid json value"

jstring :: Parser Text
jstring = char '"' *> jstring_

jstring_ :: Parser Text
jstring_ = T.pack <$> manyTill anyChar (char '"')
{-# INLINE jstring_ #-}

----------------------------------------------------------------------------
-- Helpers

skipSpace :: Parser ()
skipSpace = skipMany spaceChar
