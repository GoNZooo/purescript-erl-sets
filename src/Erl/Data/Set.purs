module Erl.Data.Set
  ( Set
  , fromFoldable
  , empty
  , isEmpty
  , singleton
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Erl.Data.List (List)
import Erl.Data.List as List

-- | An Erlang set as created by the `sets` module.
foreign import data Set :: Type -> Type

fromFoldable :: forall f a. Foldable f => Eq a => f a -> Set a
fromFoldable = List.fromFoldable >>> fromList_

-- | An empty set
empty :: forall a. Set a
empty = empty_

-- | Test if a set is empty
isEmpty :: forall a. Set a -> Boolean
isEmpty s = isEmpty_ s

-- | Create a set with one element
singleton :: forall a. a -> Set a
singleton a = singleton_ a

foreign import fromList_ :: forall a. List a -> Set a

foreign import empty_ :: forall a. Set a

foreign import isEmpty_ :: forall a. Set a -> Boolean

foreign import singleton_ :: forall a. a -> Set a
