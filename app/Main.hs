module Main where

import Data.Csv (decode, HasHeader(NoHeader))
import Data.Foldable (toList)
import Vong (Replacement, translate)
import qualified Data.ByteString.Lazy as B (ByteString, readFile)
import qualified Data.Text.IO as T (getContents, putStrLn)

parseReplacements :: B.ByteString -> [Replacement]
parseReplacements csvData =
    case decode NoHeader csvData of
        Right xs -> toList xs
        Left _ -> []

main :: IO ()
main = do
    repls <- parseReplacements <$> B.readFile "vong.csv"
    T.getContents >>= translate repls >>= T.putStrLn

