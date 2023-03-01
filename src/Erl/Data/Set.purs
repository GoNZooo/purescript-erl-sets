-- | A module implementing Erlang sets as created by the `sets` module with `{version, 2}`.
module Erl.Data.Set
  ( Set
  , fromFoldable
  , empty
  , isEmpty
  , singleton
  , toUnfoldable
  , toList
  , fromList
  , union
  , union'
  , insert
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Unfoldable (class Unfoldable)
import Erl.Data.List (List, nil, (:))
import Erl.Data.List as List
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | An Erlang set as created by the `sets` module with the `{version, 2}` option passed.
foreign import data Set :: Type -> Type

instance eqSet :: Eq a => Eq (Set a) where
  eq set1 set2 = eq_ set1 set2

instance showSet :: Show a => Show (Set a) where
  show set = "fromFoldable " <> show (toUnfoldable set :: Array a)

instance arbitrarySet :: (Arbitrary a, Eq a) => Arbitrary (Set a) where
  arbitrary = do
    (xs :: Array a) <- arbitrary
    xs # fromFoldable # pure

instance semigroupSet :: Semigroup (Set a) where
  append s1 s2 = union s1 s2

instance monoidSet :: Eq a => Monoid (Set a) where
  mempty = empty

-- | Creates an empty set with `{version, 2}`.
empty :: forall a. Eq a => Set a
empty = empty_

-- | Tests if a set is empty.
isEmpty :: forall a. Set a -> Boolean
isEmpty s = isEmpty_ s

-- | Creates a set with one element.
singleton :: forall a. Eq a => a -> Set a
singleton a = singleton_ a

-- | Creates a set from a list.
fromList :: forall a. Eq a => List a -> Set a
fromList l = fromList_ l

-- | Creates a list from a set.
toList :: forall a. Set a -> List a
toList s = toList_ s

-- | Creates the union of two sets.
union :: forall a. Set a -> Set a -> Set a
union s1 s2 = union_ (s1 : s2 : nil)

-- | Creates the union of all sets in a list.
union' :: forall a. List (Set a) -> Set a
union' sets = union_ sets

-- | Adds an element to a set.
insert :: forall a. Eq a => a -> Set a -> Set a
insert a s = insert_ a s

fromFoldable :: forall f a. Foldable f => Eq a => f a -> Set a
fromFoldable = List.fromFoldable >>> fromList_

toUnfoldable :: forall f a. Unfoldable f => Set a -> f a
toUnfoldable = toList >>> List.toUnfoldable

foreign import fromList_ :: forall a. List a -> Set a

foreign import empty_ :: forall a. Set a

foreign import isEmpty_ :: forall a. Set a -> Boolean

foreign import singleton_ :: forall a. a -> Set a

foreign import toList_ :: forall a. Set a -> List a

foreign import eq_ :: forall a. Set a -> Set a -> Boolean

foreign import union_ :: forall a. List (Set a) -> Set a

foreign import insert_ :: forall a. a -> Set a -> Set a
