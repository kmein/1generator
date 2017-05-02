{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Vong where

import Control.Monad (foldM)
import System.Random (randomRIO)
import qualified Data.Text as T

type Probability = Float

data Replacement = Replacement
    { search :: T.Text
    , replace :: T.Text
    , probability :: Probability
    }

conditioned :: Probability -> IO Bool
conditioned p = (1 - p <=) <$> randomRIO (0, 1)

replaceConditioned :: T.Text -> Replacement -> IO T.Text
replaceConditioned text Replacement{..} = do
    should <- conditioned probability
    return $
        if should
            then T.replace search replace text
            else text

translate :: T.Text -> IO T.Text
translate text = foldM replaceConditioned text replacements
  where
    replacements =
        [ Replacement " eine " " 1 " 1
        , Replacement " ein " " 1 " 1
        , Replacement " zwei " "2 " 1
        , Replacement " drei " "3 " 1
        , Replacement " vier " "4 " 1
        , Replacement " fünf " "5 " 1
        , Replacement " sechs " "6 " 1
        , Replacement " sieben " "6 " 1
        , Replacement " acht " "6 " 1
        , Replacement " neun " "6 " 1
        , Replacement " zehn " "6 " 1
        , Replacement "Eine " " 1 " 1
        , Replacement "Ein " " 1 " 1
        , Replacement "Zwei " "2 " 1
        , Replacement "Drei " "3 " 1
        , Replacement "Vier " "4 " 1
        , Replacement "Fünf " "5 " 1
        , Replacement "Sechs " "6 " 1
        , Replacement "Sieben " "6 " 1
        , Replacement "Acht " "6 " 1
        , Replacement "Neun " "6 " 1
        , Replacement "Zehn " "6 " 1
        , Replacement "ä" "e" 0.5
        , Replacement "ö" "öh" 0.5
        , Replacement "ü" "üh" 0.5
        , Replacement "ins" "in" 0.5
        , Replacement "sisch" "schis" 0.5
        , Replacement "stisch" "schtis" 0.5
        , Replacement "ar" "ahr" 0.5
        , Replacement "ck" "k" 0.5
        , Replacement "en" "n" 0.2
        , Replacement "ft" "f" 0.5
        , Replacement "hn" "n" 0.5
        , Replacement "m" "n" 0.8 -- TODO Nur innerhalb eines Wortes erlauben
        , Replacement "n" "m" 0.8 -- TODO Nur innerhalb eines Wortes erlauben
        , Replacement "ntsch" "nsch" 0.5
        , Replacement "ph" "f" 0.5
        , Replacement "sch" "shc" 0.5
        , Replacement "sh" "sch" 0.5
        , Replacement "st" "s" 0.5
        , Replacement "ß" "s" 0.5
        , Replacement "tt" "td" 0.5
        , Replacement "th" "tt" 0.5
        , Replacement "tzt" "ts" 0.5
        , Replacement "tz" "ts" 0.5
        , Replacement "ur" "uhr" 0.5
        , Replacement "v" "f" 0.5
        , Replacement "v" "w" 0.5
        , Replacement "," "" 1
        , Replacement "seid " "seit " 0.5
        , Replacement "al " "ahl " 0.5
        , Replacement "aber" "aba" 0.5
        , Replacement " an " " in " 0.5
        , Replacement "auen " "aun " 0.5
        , Replacement "on " "ong " 0.8
        , Replacement "y " "i " 0.5
        , Replacement "chnet " "chnen " 0.3
        , Replacement "chnen " "chnet " 0.3]
