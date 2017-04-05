{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module GHC.Generics.Lift (
    GLift(..)
  , genericLift0
  , genericLift1
  , genericLift2
  , genericLift3
  ) where

import           GHC.Generics

genericLift0
    :: forall a c. (Generic a, GLift c (Rep a))
    => (forall b. c b => b)
    -> a
genericLift0 x = to (glift0 @c x)

genericLift1
    :: forall a c. (Generic a, GLift c (Rep a))
    => (forall b. c b => b -> b)
    -> a -> a
genericLift1 f x = to (glift1 @c f (from x))

genericLift2
    :: forall a c. (Generic a, GLift c (Rep a))
    => (forall b. c b => b -> b -> b)
    -> a -> a -> a
genericLift2 f x y = to (glift2 @c f (from x) (from y))

genericLift3
    :: forall a c. (Generic a, GLift c (Rep a))
    => (forall b. c b => b -> b -> b -> b)
    -> a -> a -> a -> a
genericLift3 f x y z = to (glift3 @c f (from x) (from y) (from z))

class GLift c f where
    glift0 :: (forall a. c a => a) -> f p
    glift1 :: (forall a. c a => a -> a) -> f p -> f p
    glift2 :: (forall a. c a => a -> a -> a) -> f p -> f p -> f p
    glift3 :: (forall a. c a => a -> a -> a -> a) -> f p -> f p -> f p -> f p


instance GLift c f => GLift c (M1 i d f) where
    glift0 x = M1 (glift0 @c x)
    glift1 f (M1 x) = M1 (glift1 @c f x)
    glift2 f (M1 x) (M1 y) = M1 (glift2 @c f x y)
    glift3 f (M1 x) (M1 y) (M1 z) = M1 (glift3 @c f x y z)


instance (GLift c f, GLift c g) => GLift c (f :*: g) where
    glift0 x = glift0 @c x :*: glift0 @c x
    glift1 f (x :*: y) = glift1 @c f x :*: glift1 @c f y 
    glift2 f (x1 :*: y1) (x2 :*: y2) = glift2 @c f x1 x2 :*: glift2 @c f y1 y2
    glift3 f (x1 :*: y1) (x2 :*: y2) (x3 :*: y3) = glift3 @c f x1 x2 x3 :*: glift3 @c f y1 y2 y3

instance c a => GLift c (K1 i a) where
    glift0 x = K1 x
    glift1 f (K1 x) = K1 (f x)
    glift2 f (K1 x) (K1 y) = K1 (f x y)
    glift3 f (K1 x) (K1 y) (K1 z) = K1 (f x y z)

instance GLift c U1 where
    glift0 _ = U1
    glift1 _ _ = U1
    glift2 _ _ _ = U1
    glift3 _ _ _ _ = U1
