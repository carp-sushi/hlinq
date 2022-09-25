module HLINQ (
  Query (..),
  runQuery,
  select_,
  join_,
  where_,
) where

import Control.Applicative
import Control.Monad

-- Extract fields from a row.
select_ :: (Monad m) => (a -> b) -> m a -> m b
select_ = fmap

-- Combine two rows when they have a value in common.
join_ ::
  (Monad m, Alternative m, Eq c) =>
  m a ->
  (a -> c) ->
  m b ->
  (b -> c) ->
  m (a, b)
join_ m1 f1 m2 f2 = do
  a <- m1
  b <- m2
  guard $ f1 a == f2 b
  return (a, b)

-- Filter rows based on a predicate.
where_ :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
where_ fn ma = do
  a <- ma
  guard $ fn a
  return a

-- Describes how to run a HLINQ query.
hlinq_ :: (b -> c) -> a -> (a -> b) -> c
hlinq_ s j w = (s . w) j

-- A wrapper that allows for specifying a query without a where clause.
data Query m a b
  = Query (m a -> m b) (m a) (m a -> m a)
  | Query_ (m a -> m b) (m a)

-- Runs HLINQ queries.
runQuery :: (Monad m, Alternative m) => Query m a b -> m b
runQuery (Query s j w) = hlinq_ s j w
runQuery (Query_ s j) = hlinq_ s j (where_ $ const True)
