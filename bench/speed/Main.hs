module Main (main) where

import Control.DeepSeq
import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString             as B
import qualified ParsersBench.CSV.Attoparsec as A
import qualified ParsersBench.CSV.Megaparsec as M

main :: IO ()
main = defaultMain
  [ bgroup "CSV (Attoparsec)"
    [ bparser file A.parseCSV | file <- csvFiles ]
  , bgroup "CSV (Megaparsec)"
    [ bparser file M.parseCSV | file <- csvFiles ]
  ]

bparser :: NFData a => FilePath -> (ByteString -> a) -> Benchmark
bparser desc f = env (B.readFile path) (bench desc . nf f)
  where
    path = "bench-data/" ++ desc

csvFiles :: [FilePath]
csvFiles =
  [ "csv-5.csv"
  , "csv-10.csv"
  , "csv-20.csv"
  , "csv-40.csv" ]
