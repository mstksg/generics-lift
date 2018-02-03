{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Monoid.Generic (
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

