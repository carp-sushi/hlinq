module Main where

import HLINQ

import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

data RowA = RowA
  { aId :: Int
  , aName :: String
  }
  deriving (Eq, Show)

data RowB = RowB
  { bId :: Int
  , bFk :: Int
  , bActive :: Bool
  }
  deriving (Eq, Show)

-- ID select: just return all rows from a table.
selectAll :: [a] -> [a]
selectAll table =
  runQuery $
    Query_ (select_ id) table

-- Testing select_ and where_
selectEvens :: [RowA] -> [String]
selectEvens table =
  runQuery $
    Query (select_ aName) table (where_ $ even . aId)

-- Testing where_
selectActives :: [RowB] -> [RowB]
selectActives table =
  runQuery $
    Query (select_ id) table (where_ bActive)

-- Testing join_
joinTables :: [RowA] -> [RowB] -> [(RowA, RowB)]
joinTables a b =
  runQuery $
    Query_
      (select_ id)
      (join_ a aId b bFk)

-- Testing join_ with where_
joinTablesActive :: [RowA] -> [RowB] -> [String]
joinTablesActive a b =
  runQuery $
    Query
      (select_ $ aName . fst)
      (join_ a aId b bFk)
      (where_ $ bActive . snd)

-- A test table
tableA :: [RowA]
tableA =
  [ RowA 1 "row 1"
  , RowA 2 "row 2"
  , RowA 3 "row 3"
  , RowA 4 "row 4"
  ]

-- Another test table
tableB :: [RowB]
tableB =
  [ RowB 4 1 True
  , RowB 3 2 False
  , RowB 2 3 True
  , RowB 1 4 False
  ]

-- Specs for select_
spec_select =
  describe "select_" $ do
    it "selects all rows" $
      selectAll tableA `shouldBe` tableA
    it "selects names with even id values" $
      selectEvens tableA `shouldBe` ["row 2", "row 4"]
    it "selects active rows" $
      selectActives tableB `shouldBe` [RowB 4 1 True, RowB 2 3 True]

-- Specs for join_
spec_join =
  describe "join_" $ do
    it "joins tables" $
      joinTables tableA tableB `shouldBe` zip tableA tableB
    it "joins active rows" $
      joinTablesActive tableA tableB `shouldBe` ["row 1", "row 3"]

-- Collect all specs
allSpecs =
  [ spec_select
  , spec_join
  ]

-- Run tests
main :: IO ()
main = do
  specs <- concat <$> mapM testSpecs allSpecs
  defaultMain (testGroup "Specs" specs)
