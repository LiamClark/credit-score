{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE ScopedTypeVariables#-}

module Lib
    ( someFunc
    ) where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Coerce
import Data.Monoid
import Data.Foldable
import qualified Data.Vector as V
import Data.Bifuctor

someFunc :: IO ()
someFunc = do
    csvData <- BL.readFile "credit.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ show $ target p

data CreditScore = CreditScore {
     target :: !Bool
    ,creditAmount :: !Int
}

nme :: V.Vector CreditScore -> Int -> Bool
nme dataSet = \c -> credibleMean `dist` fromIntegral c < incredibleMeme `dist` fromIntegral c
    where 
         umap f = bimap f f
         (credibleMean, incredibleMeme) = umap mean (V.partition target dataSet)

mean :: V.Vector CreditScore -> Double
mean xs =  (fromIntegral total) / (fromIntegral  (length xs))
     where total :: Int = coerce $ foldMap (Sum . creditAmount) xs 

dist :: Double -> Double -> Double
dist = (abs .) . (-)


instance FromNamedRecord CreditScore where
    parseNamedRecord r =  do
           classTag :: String <-  r .: "class"
           creditAmount <- r .: "credit_amount"
           pure $ CreditScore (classTag == "good") creditAmount

