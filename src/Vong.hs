{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Vong where

import Control.Monad (foldM)
import Data.Csv (FromRecord)
import Data.List (isPrefixOf)
import GHC.Generics (Generic)
import System.Random (randomRIO)

type Probability = Float

data Replacement = Replacement
    { search :: String
    , replace :: String
    , probability :: Probability
    } deriving (Generic)

instance FromRecord Replacement

conditioned :: Probability -> IO Bool
conditioned p = (1 - p <=) <$> randomRIO (0, 1)

replaceConditioned :: String -> Replacement -> IO String
replaceConditioned [] _ = return []
replaceConditioned text@(x:xs) r@Replacement {..} =
    if isPrefixOf search text
        then do
            should <- conditioned probability
            if should
                then (replace ++) <$>
                     replaceConditioned (drop (length search) text) r
                else (x :) <$> replaceConditioned xs r
        else (x :) <$> replaceConditioned xs r

translate :: [Replacement] -> String -> IO String
translate = flip (foldM replaceConditioned)
