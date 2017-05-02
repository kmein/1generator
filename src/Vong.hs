{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Vong where

import Control.Monad (foldM)
import Data.Csv (FromRecord)
import GHC.Generics (Generic)
import System.Random (randomRIO)
import qualified Data.Text as T (Text, replace)

type Probability = Float

data Replacement = Replacement
    { search :: T.Text
    , replace :: T.Text
    , probability :: Probability
    } deriving (Generic)

instance FromRecord Replacement

conditioned :: Probability -> IO Bool
conditioned p = (1 - p <=) <$> randomRIO (0, 1)

replaceConditioned :: T.Text -> Replacement -> IO T.Text
replaceConditioned text Replacement{..} = do
    should <- conditioned probability
    return $
        if should
            then T.replace search replace text
            else text

translate :: [Replacement] -> T.Text -> IO T.Text
translate = flip (foldM replaceConditioned)
