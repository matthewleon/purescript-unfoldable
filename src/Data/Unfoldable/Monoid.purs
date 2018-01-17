module Data.Unfoldable.Monoid where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)

newtype Monoid a phantom = Monoid a

instance newtypeMonoid :: Newtype (Monoid a phantom) a
  where
  wrap = Monoid
  unwrap (Monoid x) = x

instance showMonoid :: Show a => Show (Monoid a phantom) where
  show (Monoid x) = "Monoid (" <> show x <> ")"

instance semigroupMonoid :: Semigroup a => Semigroup (Monoid a phantom) where
  append (Monoid x) (Monoid y) = Monoid (x <> y)

instance monoidMonoid :: Monoid a => Monoid (Monoid a phantom) where
  mempty = Monoid mempty

instance unfoldableMonoid :: Monoid a => Unfoldable (Monoid a) where
  unfoldr f s =
    case f s of
         Nothing           -> mempty
         Just (Tuple x s') -> Monoid x <> unfoldr f s'
