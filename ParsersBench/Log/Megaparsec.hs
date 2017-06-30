module ParsersBench.Log.Megaparsec
  ( parseLog )
where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Time
import Data.Void
import ParsersBench.Log.Common
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Lexer as L

type Parser = Parsec Void ByteString

parseLog :: ByteString -> Log
parseLog bs =
  case parse logParser "" bs of
    Left err -> error (parseErrorPretty err)
    Right x -> x

parseIP :: Parser IP
parseIP = do
  d1 <- fromIntegral <$> L.decimal
  void (char '.')
  d2 <- fromIntegral <$> L.decimal
  void (char '.')
  d3 <- fromIntegral <$> L.decimal
  void (char '.')
  d4 <- fromIntegral <$> L.decimal
  return (IP d1 d2 d3 d4)

timeParser :: Parser LocalTime
timeParser = do
  y  <- count 4 digitChar
  void (char '-')
  mm <- count 2 digitChar
  void (char '-')
  d  <- count 2 digitChar
  void (char ' ')
  h  <- count 2 digitChar
  void (char ':')
  m  <- count 2 digitChar
  void (char ':')
  s  <- count 2 digitChar
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
  void (char ' ')
  ip <- parseIP
  void (char ' ')
  p <- productParser
  return (LogEntry t ip p)

logParser :: Parser Log
logParser = many (logEntryParser <* eol)
