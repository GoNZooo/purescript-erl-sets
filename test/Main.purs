module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Erl.Data.Set (Set)
import Erl.Data.Set as Set
import Erl.Test.EUnit (suite, test)
import Erl.Test.EUnit as EUnit
import Test.Assert (assertEqual)
import Test.QuickCheck (quickCheck, (===))

main :: Effect Unit
main = do
  void $ EUnit.runTests do
    suite "set operations" do
      test "`empty`" do
        assertEqual { actual: (Set.empty :: Set Int) # Set.toUnfoldable, expected: [] }

      test "`singleton`" do
        quickCheck \(x :: Int) -> do
          (x # Set.singleton # Set.toUnfoldable) === [ x ]
