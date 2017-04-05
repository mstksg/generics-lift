{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module GHC.Generics.Numeric (
  -- * Num
    genericPlus
  , genericMinus
  , genericTimes
  , genericNegate
  , genericAbs
  , genericSignum
  , genericFromInteger
  -- * Fractional
  , genericDivide
  , genericRecip
  , genericFromRational
  ) where

import           GHC.Generics
import           GHC.Generics.Lift

genericPlus
    :: forall a. (Generic a, GLift Num (Rep a))
    => a -> a -> a
genericPlus = genericLift2 @a @Num (+)

genericMinus
    :: forall a. (Generic a, GLift Num (Rep a))
    => a -> a -> a
genericMinus = genericLift2 @a @Num (-)

genericTimes
    :: forall a. (Generic a, GLift Num (Rep a))
    => a -> a -> a
genericTimes = genericLift2 @a @Num (*)

genericNegate
    :: forall a. (Generic a, GLift Num (Rep a))
    => a -> a
genericNegate = genericLift1 @a @Num negate

genericAbs
    :: forall a. (Generic a, GLift Num (Rep a))
    => a -> a
genericAbs = genericLift1 @a @Num abs

genericSignum
    :: forall a. (Generic a, GLift Num (Rep a))
    => a -> a
genericSignum = genericLift1 @a @Num signum

genericFromInteger
    :: forall a. (Generic a, GLift Num (Rep a))
    => Integer -> a
genericFromInteger x = genericLift0 @a @Num (fromInteger x)

genericDivide
    :: forall a. (Generic a, GLift Fractional (Rep a))
    => a -> a -> a
genericDivide = genericLift2 @a @Fractional (/)

genericRecip
    :: forall a. (Generic a, GLift Fractional (Rep a))
    => a -> a
genericRecip = genericLift1 @a @Fractional recip

genericFromRational
    :: forall a. (Generic a, GLift Fractional (Rep a))
    => Rational -> a
genericFromRational x = genericLift0 @a @Fractional (fromRational x)

