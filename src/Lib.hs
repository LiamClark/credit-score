{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE ScopedTypeVariables#-}

module Lib
    ( someFunc
    ) where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V


someFunc :: IO ()
someFunc = do
    csvData <- BL.readFile "credit.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ show $ target p

data CreditScore = CreditScore {
    target :: !Bool
}

instance FromNamedRecord CreditScore where
    parseNamedRecord r =  do
           classTag :: String <-  r .: "class"
           pure $ CreditScore $ classTag == "good"

