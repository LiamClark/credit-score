module Validation where

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

-- splitInToClasses :: Eq b => (a -> b) -> Vector a -> Vector Vector a
-- splitInToClasses label dataSet =



crossValidation :: Eq b =>  Int -> Vector a -> (a -> b) -> (a -> b) -> Double
crossValidation k dataSet classifier oracle = undefined
    where folds = makeFolds k dataSet

data Fold a = MkFold {
    test :: Vector a,
    train :: Vector a
}

makeFolds :: Int -> Vector a ->  [Fold a]
makeFolds k dataSet = (\k -> split (k * stride) stride dataSet)  <$> [0..k-1]
  where stride =  V.length dataSet `div` k

split :: Int -> Int -> Vector a -> Fold a
split start length dataSet = MkFold validation (trainStart V.++ trainEnd)
    where validation = V.slice start length dataSet
          trainStart = V.take start dataSet
          trainEnd = V.drop (start + length) dataSet


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