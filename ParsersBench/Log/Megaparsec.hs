{-# LANGUAGE OverloadedStrings #-}

module ParsersBench.Log.Megaparsec
  ( parseLog )
where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Char (chr)
import Data.Time
import Data.Void
import Data.Word (Word8)
import ParsersBench.Log.Common
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Data.ByteString as B

type Parser = Parsec Void ByteString

parseLog :: ByteString -> Log
parseLog bs =
  case parse logParser "" bs of
    Left err -> error (parseErrorPretty err)
    Right x -> x

parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  void (char 46)
  d2 <- decimal
  void (char 46)
  d3 <- decimal
  void (char 46)
  d4 <- decimal
  return (IP d1 d2 d3 d4)

timeParser :: Parser LocalTime
timeParser = do
  y  <- fmap byteToChar <$> count 4 digitChar
  void (char 45)
  mm <- fmap byteToChar <$> count 2 digitChar
  void (char 45)
  d  <- fmap byteToChar <$> count 2 digitChar
  void (char 32)
  h  <- fmap byteToChar <$> count 2 digitChar
  void (char 58)
  m  <- fmap byteToChar <$> count 2 digitChar
  void (char 58)
  s  <- fmap byteToChar <$> count 2 digitChar
  return LocalTime
    { localDay       = fromGregorian (read y) (read mm) (read d)
    , localTimeOfDay = TimeOfDay (read h) (read m) (read s)
    }

productParser :: Parser Product
productParser =
      (Mouse    <$ string "mouse")
  <|> (Keyboard <$ string "keyboard")
  <|> (Monitor  <$ string "monitor")
  <|> (Speakers <$ string "speakers")

logEntryParser :: Parser LogEntry
logEntryParser = do
  t <- timeParser
  void (char 32)
  ip <- parseIP
  void (char 32)
  p <- productParser
  return (LogEntry t ip p)

logParser :: Parser Log
logParser = many (logEntryParser <* eol)

byteToChar :: Word8 -> Char
byteToChar = chr . fromIntegral

-- NOTE To be tuned for performance in Megaparec itself. Just adding this
-- here to get a picture of what sort of performance we'll have later in
-- "Text.Megaparsec.Byte.Lexer".

decimal :: Integral a => Parser a
decimal = B.foldl' step 0 `fmap` takeWhile1P (Just "digit") isDigit_w8
  where step a w = a * 10 + fromIntegral (w - 48)
{-# SPECIALISE decimal :: Parser Word8 #-}

-- | A fast digit predicate.
isDigit_w8 :: Word8 -> Bool
isDigit_w8 w = w - 48 <= 9
{-# INLINE isDigit_w8 #-}
