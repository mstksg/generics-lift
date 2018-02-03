{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.Monoid.Generics
-- Description : Derived methods for Semigroup and Monoid.
-- Copyright   : (c) Justin Le 2018
-- License     : BSD-3
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- Derived methods for Semigroup and Monoid.
--
-- Can be used for any types (deriving 'Generic') made with a single
-- constructor, where every field is an instance of 'Semigroup' (or
-- 'Monoid', depending on the function).
--
-- Also includes a newtype wrapper that imbues any such data type with
-- instant 'Semigroup' and 'Monoid' instances.
--
-- See README for details on usage instructions and motivations.
--


module Data.Monoid.Generics (
  -- * Newtype wrapper
    GMonoid(..)
  -- * Generics-derived methods  
  -- ** Semigroup
  , genericSemigroup
  -- ** Monoid
  , genericMappend
  , genericMempty
  ) where

import           Data.Data
import           Data.Semigroup
import           GHC.Generics
import           GHC.Generics.Lift

-- | If @a@ is a data type with a single constructor whose fields are all
-- instances of 'Semigroup', then @'GMonoid' a@ has a 'Semigroup' instance.
--
-- If @a@ is a data type with a single constructor whose fields are all
-- instances of 'Monoid', then @'GMonoid' a@ has a 'Monoid' instance.
--
newtype GMonoid a = GMonoid { getGMonoid :: a }
  deriving (Eq, Ord, Show, Read, Data, Generic, Functor, Foldable, Traversable)

instance GLift Semigroup (Rep (GMonoid a)) => Semigroup (GMonoid a) where
    (<>) = genericSemigroup

instance ( GLift Semigroup (Rep (GMonoid a))
         , GLift Monoid (Rep (GMonoid a))
         )
      => Monoid (GMonoid a) where
    mappend = genericMappend
    mempty  = genericMempty


genericSemigroup
    :: forall a. (Generic a, GLift Semigroup (Rep a))
    => a -> a -> a
genericSemigroup = genericLift2 @a @Semigroup (<>)

genericMappend
    :: forall a. (Generic a, GLift Monoid (Rep a))
    => a -> a -> a
genericMappend = genericLift2 @a @Monoid mappend

genericMempty
    :: forall a. (Generic a, GLift Monoid (Rep a))
    => a
genericMempty = genericLift0 @a @Monoid mempty

