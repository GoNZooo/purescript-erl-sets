module Test.Main
  ( main
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Effect (Effect)
import Erl.Data.List (nil, (:))
import Erl.Data.Set (Set)
import Erl.Data.Set as Set
import Erl.Test.EUnit (suite, test)
import Erl.Test.EUnit as EUnit
import Test.Assert (assertEqual)
import Test.QuickCheck (class Testable, Result(..), quickCheck, (===))

newtype Properties = Properties (Array Result)

instance Testable Properties where
  test (Properties results) = do
    pure $ foldl keepFailure Success results
    where
    keepFailure _oldResult (Failed reason) = Failed reason
    keepFailure oldResult _newResult = oldResult

main :: Effect Unit
main = do
  void $ EUnit.runTests do
    suite "set operations" do
      test "`empty` & `isEmpty`" do
        assertEqual { actual: (Set.empty :: Set Int) # Set.toUnfoldable, expected: [] }
        assertEqual { actual: (Set.empty :: Set Int) # Set.isEmpty, expected: true }

        quickCheck \(x :: Int) -> do
          (x # Set.singleton # Set.isEmpty) === false

      test "`singleton`" do
        quickCheck \(x :: Int) -> do
          Set.singleton x === Set.fromFoldable [ x ]

      test "`union` & `union'`" do
        quickCheck \(xs :: Set Int) -> do
          Set.union xs xs === xs

        quickCheck \(xs :: Set Int) (ys :: Set Int) -> do
          Set.union xs ys === Set.union ys xs

        quickCheck \(xs :: Set Int) (ys :: Set Int) (zs :: Set Int) -> do
          Set.union xs (Set.union ys zs) === Set.union (Set.union xs ys) zs

        quickCheck \(xs :: Set Int) (ys :: Set Int) -> do
          Set.union xs ys === Set.union' (xs : ys : nil)

      test "`Semigroup` & `Monoid`" do
        quickCheck \(xs :: Set Int) -> do
          xs <> xs === xs

        quickCheck \(xs :: Set Int) (ys :: Set Int) -> do
          xs <> ys === ys <> xs

        quickCheck \(xs :: Set Int) (ys :: Set Int) (zs :: Set Int) -> do
          xs <> (ys <> zs) === (xs <> ys) <> zs

        quickCheck \(xs :: Set Int) (ys :: Set Int) -> do
          xs <> ys === ys <> xs

        quickCheck \(xs :: Set Int) -> do
          mempty <> xs === xs

        quickCheck \(xs :: Set Int) -> do
          xs <> mempty === xs

      test "`insert`" do
        quickCheck \(xs :: Set Int) (x :: Int) -> do
          Properties
            [ Set.insert x xs === Set.union (Set.singleton x) xs
            , (xs # Set.insert x # Set.insert x) === Set.union (Set.singleton x) xs
            , (xs # Set.insert x # Set.insert x # Set.insert x) === Set.insert x xs
            ]

      test "`delete`" do
        quickCheck \(xs :: Set Int) (x :: Int) -> do
          Properties
            [ (xs # Set.insert x # Set.delete x) === xs
            , (xs # Set.delete x # Set.delete x) === xs
            ]

      test "`filter`" do
        quickCheck \(xs :: Array Int) -> do
          (xs # Set.fromFoldable # Set.filter (const true)) === Set.fromFoldable xs

        quickCheck \(xs :: Array Int) -> do
          (xs # Set.fromFoldable # Set.filter (_ > 0)) ===
            (xs # Array.filter (_ > 0) # Set.fromFoldable)

        quickCheck \(xs :: Set Int) -> do
          Set.filter (const false) xs === Set.empty

      test "`difference`" do
        quickCheck \(xs :: Set Int) -> do
          Set.difference xs xs === Set.empty

        quickCheck \(xs :: Set Int) (ys :: Set Int) -> do
          Set.difference xs ys === Set.difference xs (Set.union xs ys)

        quickCheck \(xs :: Set Int) (ys :: Set Int) -> do
          Set.difference xs (xs <> ys) === ys

        quickCheck \(xs :: Set Int) (ys :: Set Int) -> do
          Set.difference ys (xs <> ys) === xs

