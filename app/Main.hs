module Main where

import Vong
import qualified Data.Text.IO as T

main :: IO ()
main = T.getContents >>= translate >>= T.putStrLn
