{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Monoid.Generic (
    genericSemigroup
  , genericMappend
  , genericMempty
  ) where

import           Data.Semigroup
import           GHC.Generics
import           GHC.Generics.Lift

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

