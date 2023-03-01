module Test.Main
  ( main
  ) where

import Prelude

import Data.Array as Array
import Effect (Effect)
import Erl.Data.List (nil, (:))
import Erl.Data.Set (Set)
import Erl.Data.Set as Set
import Erl.Test.EUnit (suite, test)
import Erl.Test.EUnit as EUnit
import Test.Assert (assertEqual)
import Test.QuickCheck ((===))
import Test.QuickCheck.Helpers (Properties(..), property)

main :: Effect Unit
main = do
  void $ EUnit.runTests do
    suite "set operations" do
      test "`empty` & `isEmpty`" do
        assertEqual { actual: (Set.empty :: Set Int) # Set.toUnfoldable, expected: [] }
        assertEqual { actual: (Set.empty :: Set Int) # Set.isEmpty, expected: true }

        property "Sets created with `singleton` are never empty" \(x :: Int) -> do
          (x # Set.singleton # Set.isEmpty) === false

      test "`singleton`" do
        property "`singleton` is the same as `fromFoldable` with one element" \(x :: Int) -> do
          Set.singleton x === Set.fromFoldable [ x ]

      test "`union` & `union'`" do
        property "The union of two identical sets is `identity`" \(xs :: Set Int) -> do
          Set.union xs xs === xs

        property "Argument order does not matter for `union`" \(xs :: Set Int) (ys :: Set Int) -> do
          Set.union xs ys === Set.union ys xs

        property "Associativity for `union`" \(xs :: Set Int) (ys :: Set Int) (zs :: Set Int) -> do
          Set.union xs (Set.union ys zs) === Set.union (Set.union xs ys) zs

        property "`union` works the same as `unions`" \(xs :: Set Int) (ys :: Set Int) -> do
          Set.union xs ys === Set.unions (xs : ys : nil)

      test "`Semigroup` & `Monoid`" do
        property "Appending the same set together with itself is `identity`" \(xs :: Set Int) -> do
          xs <> xs === xs

        property "Argument order does not matter for `<>`" \(xs :: Set Int) (ys :: Set Int) -> do
          xs <> ys === ys <> xs

        property "Associativity" \(xs :: Set Int) (ys :: Set Int) (zs :: Set Int) -> do
          xs <> (ys <> zs) === (xs <> ys) <> zs

        property "Left identity" \(xs :: Set Int) -> do
          mempty <> xs === xs

        property "Right identity" \(xs :: Set Int) -> do
          xs <> mempty === xs

      test "`insert`" do
        property "Element insertion is idempotent" \(xs :: Set Int) (x :: Int) -> do
          Properties
            [ Set.insert x xs === Set.union (Set.singleton x) xs
            , (xs # Set.insert x # Set.insert x) === Set.union (Set.singleton x) xs
            , (xs # Set.insert x # Set.insert x # Set.insert x) === Set.insert x xs
            ]

      test "`delete`" do
        property "Element deletion is idempotent" \(xs :: Set Int) (x :: Int) -> do
          Properties
            [ (xs # Set.insert x # Set.delete x) === xs
            , (xs # Set.delete x # Set.delete x) === xs
            ]

      test "`filter`" do
        property "Always returning `true` keeps all elements" \(xs :: Array Int) -> do
          (xs # Set.fromFoldable # Set.filter (const true)) === Set.fromFoldable xs

        property "Filtering works like for arrays" \(xs :: Array Int) -> do
          (xs # Set.fromFoldable # Set.filter (_ > 0)) ===
            (xs # Array.filter (_ > 0) # Set.fromFoldable)

        property "Always returning `false` yields the empty set" \(xs :: Set Int) -> do
          Set.filter (const false) xs === Set.empty

      test "`difference`" do
        property "The difference between a set and itself is the empty set" \(xs :: Set Int) -> do
          Set.difference xs xs === Set.empty

        property "The difference between `xs` and `xs <> ys` is `ys`"
          \(xs :: Set Int) (ys :: Set Int) -> do
            Set.difference xs (xs <> ys) === ys

        property "The difference between `ys` and `xs <> ys` is `xs`"
          \(xs :: Set Int) (ys :: Set Int) -> do
            Set.difference ys (xs <> ys) === xs

      test "`intersection`" do
        property "The intersection of `xs` and `xs` is `identity`" \(xs :: Set Int) -> do
          Set.intersection xs xs === xs

        property "Argument order does not matter for `intersection`"
          \(xs :: Set Int) (ys :: Set Int) -> do
            Set.intersection xs ys === Set.intersection ys xs

        property "Associativity" \(xs :: Set Int) (ys :: Set Int) (zs :: Set Int) -> do
          Set.intersection xs (Set.intersection ys zs) ===
            Set.intersection (Set.intersection xs ys) zs

      test "`member`" do
        property "Inserted elements are members of a set" \(xs :: Set Int) (x :: Int) -> do
          (xs # Set.insert x # Set.member x) === true

        property "Deleted elements are not members of a set" \(xs :: Set Int) (x :: Int) -> do
          (xs # Set.delete x # Set.member x) === false
