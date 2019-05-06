{-# LANGUAGE DeriveFunctor #-}
module Lib where

import qualified Data.Map as M
import Data.Monoid
import Control.Applicative

newtype Heap m a = Heap { getHeap :: M.Map m [ a ] }
    deriving (Functor, Show)

instance (Ord m) => Semigroup (Heap m a) where
  Heap a <> Heap b = Heap $ M.unionWith (<>) a b

instance (Ord m) => Monoid (Heap m a) where
  mempty = Heap mempty

instance (Monoid m, Ord m) => Applicative (Heap m) where
  pure a = Heap $ M.singleton mempty [a]
  Heap a <*> Heap b = Heap $ M.intersectionWith go a b
    where
      go fs xs = zipWith ($) fs xs

instance (Monoid m, Ord m) => Monad (Heap m) where
  Heap m >>= f = M.foldMapWithKey go m
    where
      go k a = let h = M.unionsWith (<>) (getHeap . f <$> a)
                in Heap $ M.mapKeys (k <>) h

fromList :: (Ord m, Monoid m) => [a] -> Heap m a
fromList = foldMap pure

cost :: m -> a -> Heap m a
cost m a = Heap $ M.singleton m [a]

test :: Heap (Sum Int) String
test = do
    x <- fromList ["hi", "hello", "howareyou?", "yo"]
    cost (Sum $ length x) x
