generics-lift
=============

Gives you methods to "lift" polymorphic typeclass operations over simple
tuple/product types using GHC generics.  Can very easily be adapted to make
automatic instances for your own typeclasses.

Use cases
---------

It has three main usages:

### Free instances

For example, consider a basic tuple over an `Int` and a parameterized type:

```haskell
data Tup a b = Tup a b
  deriving Generic
```

The `Semigroup`, `Monoid`, `Num`, `Fractional`, and `Floating` instances are
all mechanical:

```haskell
instance (Num a, Num b) => Num (Tup a b) where
    Tup x1 y1 + Tup x2 y2 = Tup (x1 + x2)       (y1 + y2)
    Tup x1 y1 - Tup x2 y2 = Tup (x1 - x2)       (y1 - y2)
    Tup x1 y1 * Tup x2 y2 = Tup (x1 * x2)       (y1 * y2)
    negate (Tup x y)      = Tup (negate x)      (negate y)
    abs    (Tup x y)      = Tup (abs    x)      (abs    y)
    signum (Tup x y)      = Tup (signum x)      (signum y)
    fromInteger x         = Tup (fromInteger x) (fromInteger x)

instance (Semigroup a, Semigroup b) => Semigroup (Tup a b) where
    Tup x1 y1 <> Tup x2 y2 = Tup (x1 <> x2) (y1 <> y2)
```

But, if you derive `Generic`, this package lets you just do:

```haskell
instance (Num a, Num b) => Num (Tup a b) where
    (+)         = genericPlus
    (-)         = genericMinus
    (*)         = genericTimes
    negate      = genericNegate
    abs         = genericAbs
    signum      = genericSignum
    fromInteger = genericFromInteger

instance (Semigroup a, Semigroup b) => Semigroup (Tup a b) where
    (<>) = genericSemigroup
```

#### Newtype wrappers

Similar to `WrappedMonoid` and `WarppedMonad` from *base*, some convenient
newtype wrappers are provided that will give free instances of `Num`, etc. for
appropriate types:

If `a` is a data type (deriving `Generic`) with a single constructor whose
fields all have instances of `Num`, then `GNum a` has a `Num` instance (and
same for `Fractional`, `Floating`, etc.).

If `a` is a data type (deriving `Generic`) with a single constructor whose
fields all have instances of `Semigroup`, then `GMonoid a` has a `Semigroup`
instance (and same for `Monoid`).

### Write methods for your own classes

For classes where the methods just "combine" values of a type polymorphically,
you can write your own version of `genericPlus` etc. using `genericLift0`,
`genericLift1`, `genericLift2`, and `genericLift3`.

Here are examples for `(+)`, `negate`, and `fromInteger` from `Num`:

```haskell
genericPlus
    :: forall a. (Generic a, GLift Num (Rep a))
    => a -> a -> a
genericPlus = genericLift2 @a @Num (+)

genericNegate
    :: forall a. (Generic a, GLift Num (Rep a))
    => a -> a
genericNegate = genericLift1 @a @Num negate

genericFromInteger
    :: forall a. (Generic a, GLift Num (Rep a))
    => Integer -> a
genericFromInteger x = genericLift0 @a @Num (fromInteger x)
```

Note that is necessary to use `-XTypeApplications` (with `@a @Num` etc.) in
order to specify what typeclass it is you want to generalize.

### Default/Automatic implementations for your own classes

If you make a new typeclass, you can use this package to provide default
instances for your classes.

```haskell
class MyClass a where
    f :: String -> a
    g :: a -> a

    default f :: (Generic a, GLift MyClass (Rep a)) => String -> a
    f x = genericLift0 @a @MyClass (f x)
    
    default g :: (Generic a, GLift MyClass (Rep a)) => a -> a
    g = genericLift1 @a @MyClass g
```

Now when a user makes a new type, they can write an instance instantly by just
doing:

```haskell
instance (MyClass a, MyClass b) => MyClass (Tup a b)
```

Or, if `-XDeriveAnyClass` is enabled, they can just do:

```haskell
data Tup a b = Tup a b
  deriving (Generic, MyClass)
```

Without any boilerplate!

Scope
-----

This package will work for types (deriving `Generic`) consisting of a *single
constructor*, where every field is an instance of the typeclass being lifted.

For example, you can use `genericPlus` on types like:

```haskell
data Tup1 a b = Tup1 a b            -- requires Num a, Num b
data Tup2 a   = Tup2 Int a          -- requires Num a, Num b
data Tup3     = Tup3 Int Double
data Tup4 a b = Tup4 Int Double     -- no constraint on a or b
```

But not on, say:

```haskell
data Tup5 a   = Tup2 String a       -- String is not an instance of Num
```

Comparison
----------

There are many similar packages currently in the haskell ecosystem:

*   *[generic-deriving][]* does something similar (give default implementations
    using generics), but with an ad-hoc approach, creating a new "generic"
    typeclass for every typeclass you want to derive.  *generics-lift*,
    instead, uses a single typeclass (`GLift`) to derive functions for all
    typeclasses.  Because of this, this is more suitable for those wanting to
    leverage an existing generics system to write their default instance
    generators, instead of rigging one from scratch.

*   *[one-liner][]* does give a method for leveraging Generics to generate
    default instances for your own classes, and in a way that generalizes to
    n-ary functions (instead of just 0, 1, 2, and 3, currently, for this one).
    However, it does not seek to provide default methods for common classes
    like `Num` and `Monoid`, instead giving users the tools to create those
    methods themselves.

[one-liner]: https://hackage.haskell.org/package/one-liner
[generic-deriving]: http://hackage.haskell.org/package/generic-deriving

In a way, this package is a fusion of the functionality of both
*[generic-deriving][]* and *[one-liner][]*.
