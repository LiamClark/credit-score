{-#LANGUAGE OverloadedStrings#-}

module Lib
    ( someFunc
    ) where

import Control.Applicative
import Data.Csv


someFunc :: IO ()
someFunc = putStrLn "someFunc"

data CreditScore = CreditScore {
    target :: !Bool
}

instance FromNamedRecord CreditScore where
    parseNamedRecord r =  do
           classTag <-  r .: "class"
           pure $ CreditScore $ classTag == "good"

