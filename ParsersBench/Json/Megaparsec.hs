{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module ParsersBench.Json.Megaparsec
  ( parseJson )
where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Void
import Data.Word (Word8)
import ParsersBench.Json.Common
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Data.ByteString     as B
import qualified Data.HashMap.Strict as H
import qualified Data.Scientific     as Sci
import qualified Data.Text.Encoding  as TE
import qualified Data.Vector         as V

type Parser = Parsec Void ByteString

#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define COMMA 44
#define C_0 48
#define C_9 57
#define C_MINUS 45
#define C_f 102
#define C_n 110
#define C_t 116
#define DOUBLE_QUOTE 34
#define OPEN_CURLY 123
#define OPEN_SQUARE 91
#define COLON 58

parseJson :: ByteString -> Value
parseJson bs =
  case parse json "" bs of
    Left err -> error (parseErrorPretty err)
    Right x -> x

json :: Parser Value
json = json_ object_ array_

json_ :: Parser Value -> Parser Value -> Parser Value
json_ obj ary = do
  w <- space *> (char OPEN_CURLY <|> char OPEN_SQUARE)
  if w == OPEN_CURLY
    then obj
    else ary
{-# INLINE json_ #-}

object_ :: Parser Value
object_ = Object <$> objectValues jstring value

objectValues :: Parser Text -> Parser Value -> Parser (H.HashMap Text Value)
objectValues str val = do
  space
  let pair = liftA2 (,) (str <* space) (char COLON *> space *> val)
  H.fromList <$> commaSeparated pair CLOSE_CURLY
{-# INLINE objectValues #-}

array_ :: Parser Value
array_ = Array <$> arrayValues value

arrayValues :: Parser Value -> Parser (Vector Value)
arrayValues val = do
  space
  V.fromList <$> commaSeparated val CLOSE_SQUARE
{-# INLINE arrayValues #-}

commaSeparated :: Parser a -> Word8 -> Parser [a]
commaSeparated item endByte = do
  w <- lookAhead anyChar
  if w == endByte
    then [] <$ anyChar
    else loop
  where
    loop = do
      v <- item <* space
      ch <- char COMMA <|> char endByte
      if ch == COMMA
        then space >> (v:) <$> loop
        else return [v]
{-# INLINE commaSeparated #-}

value :: Parser Value
value = do
  w <- lookAhead anyChar
  case w of
    DOUBLE_QUOTE -> anyChar *> (String <$> jstring_)
    OPEN_CURLY   -> anyChar *> object_
    OPEN_SQUARE  -> anyChar *> array_
    C_f          -> Bool False <$ string "false"
    C_t          -> Bool True  <$ string "true"
    C_n          -> string "null" *> pure Null
    _
      | w >= C_0 && w <= C_9 || w == C_MINUS -> Number <$> scientific
        -- TODO Pfff… need proper fast L.number from Megaparsec
      | otherwise -> fail "not a valid json value"

jstring :: Parser Text
jstring = char 34 *> jstring_

jstring_ :: Parser Text
jstring_ = TE.decodeUtf8 <$>
  takeWhileP (Just "string char") (/= 34) <* char 34
{-# INLINE jstring_ #-}

-- NOTE For now it's stolen from Attoparsec, once the optimizations for
-- numeric parsers land in Megaparsec, we'll have this performance there by
-- default.

scientific :: Parser Scientific
scientific = do
  -- NOTE This can be done more elegantly, but for now I just want to check
  -- if we'll be close to Attoparsec with the literal translation.
  let minus = 45
      plus  = 43
  sign <- lookAhead anyChar
  let !positive = sign == plus || sign /= minus
  when (sign == plus || sign == minus) $
    void anyChar
  n <- decimal
  let f fracDigits = SP
        (B.foldl' step n fracDigits)
        (negate $ B.length fracDigits)
      step a w = a * 10 + fromIntegral (w - 48)
  dotty <- lookAhead (optional anyChar)
  -- '.' -> ascii 46
  SP c e <-
    case dotty of
      Just 46 -> anyChar *> (f <$> takeWhileP Nothing isDigit_w8)
      _       -> pure (SP n 0)

  let !signedCoeff | positive  =  c
                   | otherwise = -c

  let littleE = 101
      bigE    = 69
  (satisfy (\ex -> ex == littleE || ex == bigE) *>
      fmap (Sci.scientific signedCoeff . (e +)) (signed decimal)) <|>
    return (Sci.scientific signedCoeff    e)

signed :: Num a => Parser a -> Parser a
{-# SPECIALISE signed :: Parser Int -> Parser Int #-}
{-# SPECIALISE signed :: Parser Integer -> Parser Integer #-}
signed p = (negate <$> (char 45 *> p))
       <|> (char 43 *> p)
       <|> p

-- A strict pair
data SP = SP !Integer {-# UNPACK #-} !Int

decimal :: Integral a => Parser a
decimal = B.foldl' step 0 `fmap` takeWhile1P (Just "digit") isDigit_w8
  where step a w = a * 10 + fromIntegral (w - 48)
{-# SPECIALISE decimal :: Parser Word8 #-}
{-# SPECIALISE decimal :: Parser Integer #-}

-- | A fast digit predicate.
isDigit_w8 :: Word8 -> Bool
isDigit_w8 w = w - 48 <= 9
{-# INLINE isDigit_w8 #-}
