{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.Generic (
  -- * Newtype wrapper
    GNum(..)
  -- * Generics-derived methods  
  -- ** Num
  , genericPlus
  , genericMinus
  , genericTimes
  , genericNegate
  , genericAbs
  , genericSignum
  , genericFromInteger
  -- ** Fractional
  , genericDivide
  , genericRecip
  , genericFromRational
  -- ** Floating
  , genericPi
  , genericExp
  , genericLog
  , genericSqrt
  , genericPower
  , genericLogBase
  , genericSin
  , genericCos
  , genericTan
  , genericAsin
  , genericAcos
  , genericAtan
  , genericSinh
  , genericCosh
  , genericTanh
  , genericAsinh
  , genericAcosh
  , genericAtanh
  ) where

import           Data.Data
import           GHC.Generics
import           GHC.Generics.Lift

newtype GNum a = GNum { getGNum :: a }
  deriving (Eq, Ord, Show, Read, Data, Generic, Functor, Foldable, Traversable)

instance GLift Num (Rep (GNum a)) => Num (GNum a) where
    (+)         = genericPlus
    (-)         = genericMinus
    (*)         = genericTimes
    negate      = genericNegate
    abs         = genericAbs
    signum      = genericSignum
    fromInteger = genericFromInteger

instance ( GLift Num        (Rep (GNum a))
         , GLift Fractional (Rep (GNum a))
         )
      => Fractional (GNum a) where
    (/)          = genericDivide
    recip        = genericRecip
    fromRational = genericFromRational

instance ( GLift Num        (Rep (GNum a))
         , GLift Fractional (Rep (GNum a))
         , GLift Floating   (Rep (GNum a))
         )
      => Floating (GNum a) where
    pi      = genericPi
    exp     = genericExp
    log     = genericLog
    sqrt    = genericSqrt
    (**)    = genericPower
    logBase = genericLogBase
    sin     = genericSin
    cos     = genericCos
    tan     = genericTan
    asin    = genericAsin
    acos    = genericAcos
    atan    = genericAtan
    sinh    = genericSinh
    cosh    = genericCosh
    tanh    = genericTanh
    asinh   = genericAsinh
    acosh   = genericAcosh
    atanh   = genericAtanh

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

genericPi
    :: forall a. (Generic a, GLift Floating (Rep a))
    => a
genericPi = genericLift0 @a @Floating pi

genericExp
    :: forall a. (Generic a, GLift Floating (Rep a))
    => a -> a
genericExp = genericLift1 @a @Floating exp

genericLog
    :: forall a. (Generic a, GLift Floating (Rep a))
    => a -> a
genericLog = genericLift1 @a @Floating log

genericSqrt
    :: forall a. (Generic a, GLift Floating (Rep a))
    => a -> a
genericSqrt = genericLift1 @a @Floating sqrt

genericPower
    :: forall a. (Generic a, GLift Floating (Rep a))
    => a -> a -> a
genericPower = genericLift2 @a @Floating (**)

genericLogBase
    :: forall a. (Generic a, GLift Floating (Rep a))
    => a -> a -> a
genericLogBase = genericLift2 @a @Floating logBase

genericSin
    :: forall a. (Generic a, GLift Floating (Rep a))
    => a -> a
genericSin = genericLift1 @a @Floating sin

genericCos
    :: forall a. (Generic a, GLift Floating (Rep a))
    => a -> a
genericCos = genericLift1 @a @Floating cos

genericTan
    :: forall a. (Generic a, GLift Floating (Rep a))
    => a -> a
genericTan = genericLift1 @a @Floating tan

genericAsin
    :: forall a. (Generic a, GLift Floating (Rep a))
    => a -> a
genericAsin = genericLift1 @a @Floating asin

genericAcos
    :: forall a. (Generic a, GLift Floating (Rep a))
    => a -> a
genericAcos = genericLift1 @a @Floating acos

genericAtan
    :: forall a. (Generic a, GLift Floating (Rep a))
    => a -> a
genericAtan = genericLift1 @a @Floating atan

genericSinh
    :: forall a. (Generic a, GLift Floating (Rep a))
    => a -> a
genericSinh = genericLift1 @a @Floating sinh

genericCosh
    :: forall a. (Generic a, GLift Floating (Rep a))
    => a -> a
genericCosh = genericLift1 @a @Floating cosh

genericTanh
    :: forall a. (Generic a, GLift Floating (Rep a))
    => a -> a
genericTanh = genericLift1 @a @Floating atanh

genericAsinh
    :: forall a. (Generic a, GLift Floating (Rep a))
    => a -> a
genericAsinh = genericLift1 @a @Floating asinh

genericAcosh
    :: forall a. (Generic a, GLift Floating (Rep a))
    => a -> a
genericAcosh = genericLift1 @a @Floating acosh

genericAtanh
    :: forall a. (Generic a, GLift Floating (Rep a))
    => a -> a
genericAtanh = genericLift1 @a @Floating atanh
