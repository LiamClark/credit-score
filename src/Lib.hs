{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE ScopedTypeVariables#-}

module Lib where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Coerce
import Data.Monoid
import Data.Foldable
import qualified Data.Vector as V
import           Data.Vector (Vector)
import qualified Data.Vector.Mutable as MV
import           Data.Vector.Mutable (MVector)
import Data.Bifunctor

import Control.Monad.Random.Class

import qualified System.Random as Random
import Control.Monad.ST


someFunc :: IO ()
someFunc = do
    csvData <- BL.readFile "credit.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            print $ target p

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
mean xs =  (fromIntegral total) / (fromIntegral (length xs))
     where total :: Int = coerce $ foldMap (Sum . creditAmount) xs

dist :: Double -> Double -> Double
dist = (abs .) . (-)


instance FromNamedRecord CreditScore where
    parseNamedRecord r =  do
           classTag :: String <-  r .: "class"
           creditAmount <- r .: "credit_amount"
           pure $ CreditScore (classTag == "good") creditAmount



crossValidation :: Eq b =>  Int -> Vector a -> (a -> b) -> (a -> b) -> Double
crossValidation k dataSet classifier oracle = undefined
    where folds = makeFolds k dataSet

type RandomIO = IO

makeFolds :: Int -> Vector a ->  [Vector a]
makeFolds k dataSet = (\k -> V.slice (k * stride) stride dataSet)  <$> [0..k-1]
  where stride =  V.length dataSet `div` k

exampleVec :: Vector Int
exampleVec = V.fromList [1..99]


-- shuffle :: Vector a -> Random.StdGen -> (Vector a, Random.StdGen)
-- shuffle vector gen =
--   runST $ do
--     mutable <- V.thaw vector
--     gen'    <- shuffleST mutable gen
--     frozen  <- V.unsafeFreeze mutable
--     return (frozen, gen')


-- shuffleST :: MonadRandom m => MVector s a -> m (ST s ())
-- shuffleST v = loop (MV.length v)

--   where
--     loop i =
--       if i <= 1
--         then return $ return ()
--         else do
--           let i' = i - 1
--           index <- getRandomR (0, i')
--           pure $ MV.swap v i' index
--           loop i'