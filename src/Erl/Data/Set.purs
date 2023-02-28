-- | A module implementing Erlang sets as created by the `sets` module with `{version, 2}`.
module Erl.Data.Set
  ( Set
  , fromFoldable
  , empty
  , isEmpty
  , singleton
  , toUnfoldable
  , toList
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Unfoldable (class Unfoldable)
import Erl.Data.List (List)
import Erl.Data.List as List

-- | An Erlang set as created by the `sets` module with the `{version, 2}` option passed.
foreign import data Set :: Type -> Type

fromFoldable :: forall f a. Foldable f => Eq a => f a -> Set a
fromFoldable = List.fromFoldable >>> fromList_

toUnfoldable :: forall f a. Unfoldable f => Set a -> f a
toUnfoldable = toList >>> List.toUnfoldable

-- | Creates an empty set with `{version, 2}`.
empty :: forall a. Set a
empty = empty_

-- | Tests if a set is empty.
isEmpty :: forall a. Set a -> Boolean
isEmpty s = isEmpty_ s

-- | Creates a set with one element.
singleton :: forall a. a -> Set a
singleton a = singleton_ a

-- | Creates a list from a set.
toList :: forall a. Set a -> List a
toList s = toList_ s

foreign import fromList_ :: forall a. List a -> Set a

foreign import empty_ :: forall a. Set a

foreign import isEmpty_ :: forall a. Set a -> Boolean

foreign import singleton_ :: forall a. a -> Set a

foreign import toList_ :: forall a. Set a -> List a
