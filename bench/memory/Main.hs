module Main (main) where

import Control.DeepSeq
import Control.Monad
import Data.ByteString (ByteString)
import Weigh
import qualified Data.ByteString             as B
import qualified ParsersBench.CSV.Attoparsec as A
import qualified ParsersBench.CSV.Megaparsec as M
import qualified ParsersBench.Log.Attoparsec as A
import qualified ParsersBench.Log.Megaparsec as M

main :: IO ()
main = mainWith $ do
  setColumns [Case, Allocated, GCs, Max]
  forM_ csvFiles $ \file ->
    bparser "CSV (Attoparsec)" file A.parseCSV
  forM_ csvFiles $ \file ->
    bparser "CSV (Megaparsec)" file M.parseCSV
  forM_ logFiles $ \file ->
    bparser "Log (Attoparsec)" file A.parseLog
  forM_ logFiles $ \file ->
    bparser "Log (Megaparsec)" file M.parseLog

bparser :: NFData a => String -> FilePath -> (ByteString -> a) -> Weigh ()
bparser pre desc f = io (pre ++ "/" ++ desc) m path
  where
    path = "bench-data/" ++ desc
    m pth = f <$> B.readFile pth

csvFiles :: [FilePath]
csvFiles =
  [ "csv-5.csv"
  , "csv-10.csv"
  , "csv-20.csv"
  , "csv-40.csv" ]

logFiles :: [FilePath]
logFiles =
  [ "log-5.log"
  , "log-10.log"
  , "log-20.log"
  , "log-40.log" ]
