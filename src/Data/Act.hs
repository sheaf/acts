{-# LANGUAGE
    DeriveGeneric
  , DeriveDataTypeable
  , DerivingVia
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , TypeFamilies
  , UndecidableInstances
#-}

{-|
Module: Data.Act

An "Act" of a semigroup \( S \) on a type \( X \) gives a way to transform terms of type \( X \) by terms of type \( S \),
in a way that is compatible with the semigroup operation on \( S \).

In the special case that there is a unique way of going from one term of type \( X \) to another
through a transformation by a term of type \( S \), we say that \( X \) is a torsor under \( S \).

For example, the plane has an action by translations. Given any two points, there is a unique translation
that takes the first point to the second. Note that an unmarked plane (like a blank piece of paper)
has no designated origin or reference point, whereas the set of translations is a plane with a given origin
(the zero translation). This is the distinction between an affine space (an unmarked plane) and a vector space.
Enforcing this distinction in the types can help to avoid confusing absolute points with translation vectors.


Simple 'Act' and 'Torsor' instances can be derived through self-actions:

> > newtype Seconds   = Seconds { getSeconds :: Double }
> >   deriving ( Act TimeDelta, Torsor TimeDelta )
> >     via TimeDelta
> > newtype TimeDelta = TimeDelta { timeDeltaInSeconds :: Seconds }
> >   deriving ( Semigroup, Monoid, Group )
> >     via Sum Double

-}

module Data.Act
  ( Act(..)
  , transportAction
  , Trivial(..)
  , Torsor(..)
  , intertwiner
  , Finitely(..)
  )
  where

-- base
import Data.Coerce
  ( coerce )
import Data.Data
  ( Data )
import Data.Functor.Const
  ( Const(..) )
import Data.Functor.Contravariant
  ( Op(..) )
import Data.Monoid
  ( Any(..), All(..)
  , Sum(..), Product(..)
  , Ap(..), Endo(..)
  )
import Data.Semigroup
  ( Dual(..) )
import GHC.Generics
  ( Generic, Generic1 )

-- deepseq
import Control.DeepSeq
  ( NFData )

-- finitary
import Data.Finitary
  ( Finitary(..) )

-- finite-typelits
import Data.Finite
  ( Finite )

-- acts
import Data.Group
  ( Group(..), anti )

-----------------------------------------------------------------

-- | A left __act__ (or left __semigroup action__) of a semigroup @s@ on @x@ consists of an operation
-- 
-- @(•) :: s -> x -> x@
--
-- such that:
-- 
-- @a • ( b • x ) = ( a <> b ) • x@
--
-- In case @s@ is also a 'Monoid', we additionally require:
--
-- @mempty • x = x@
--
-- The synonym @ act = (•) @ is also provided.
class Semigroup s => Act s x where
  {-# MINIMAL (•) | act #-}
  -- | Left action of a semigroup.
  (•), act :: s -> x -> x
  (•) = act
  act = (•)

infixr 5 •
infixr 5 `act`

-- | Transport an act:
--
-- <<img/transport.svg>>
transportAction :: ( a -> b ) -> ( b -> a ) -> ( g -> b -> b ) -> ( g -> a -> a )
transportAction to from actBy g = from . actBy g . to

-- | Natural left action of a semigroup on itself.
instance Semigroup s => Act s s where
  (•) = (<>)

-- | Trivial act of a semigroup on any type (acting by the identity).
newtype Trivial a = Trivial { getTrivial :: a }
  deriving stock   ( Show, Read, Data, Generic, Generic1 )
  deriving newtype ( Eq, Ord, Enum, Bounded, NFData )
instance Semigroup s => Act s ( Trivial a ) where
  act _ = id

deriving via Any instance Act Any Bool
deriving via All instance Act All Bool
instance Num a => Act ( Sum     a ) a where
  act s = coerce ( act s :: Sum a -> Sum a )
instance Num a => Act ( Product a ) a where
  act s = coerce ( act s :: Product a -> Product a )

instance {-# OVERLAPPING #-} Act () x where
  act _ = id
instance ( Act s1 x1, Act s2 x2 )
      => Act ( s1, s2 ) ( x1,x2 ) where
  act ( s1, s2 ) ( x1, x2 ) =
    ( act s1 x1, act s2 x2 )
instance ( Act s1 x1, Act s2 x2, Act s3 x3 )
      => Act ( s1, s2, s3 ) ( x1, x2, x3 ) where
  act ( s1, s2, s3 ) ( x1, x2, x3 ) =
    ( act s1 x1, act s2 x2, act s3 x3 )
instance ( Act s1 x1, Act s2 x2, Act s3 x3, Act s4 x4 )
      => Act ( s1, s2, s3, s4 ) ( x1, x2, x3, x4 ) where
  act ( s1, s2, s3, s4 ) ( x1, x2, x3, x4 ) =
    ( act s1 x1, act s2 x2, act s3 x3, act s4 x4 )
instance ( Act s1 x1, Act s2 x2, Act s3 x3, Act s4 x4, Act s5 x5 )
      => Act ( s1, s2, s3, s4, s5 ) ( x1, x2, x3, x4, x5 ) where
  act ( s1, s2, s3, s4, s5 ) ( x1, x2, x3, x4, x5 ) =
    ( act s1 x1, act s2 x2, act s3 x3, act s4 x4, act s5 x5 )

deriving newtype instance Act s a => Act s ( Const a b )

-- | Acting through a functor using @fmap@.
instance ( Act s x, Functor f ) => Act s ( Ap f x ) where
  act s = coerce ( fmap ( act s ) :: f x -> f x )

-- | Acting through the contravariant function arrow functor: right action.
--
-- If acting by a group, use `anti :: Group g => g -> Dual g` to act by the original group
-- instead of the opposite group.
instance ( Semigroup s, Act s a ) => Act ( Dual s ) ( Op b a ) where
  act ( Dual s ) = coerce ( ( . act s ) :: ( a -> b ) -> ( a -> b ) )

-- | Acting through a function arrow: both covariant and contravariant actions.
--
-- If acting by a group, use `anti :: Group g => g -> Dual g` to act by the original group
-- instead of the opposite group.
instance ( Semigroup s, Act s a, Act t b ) => Act ( Dual s, t ) ( a -> b ) where
  act ( Dual s, t ) p = act t . p . act s

-- | Action of a group on endomorphisms.
instance ( Group g, Act g a ) => Act g ( Endo a ) where
  act g = coerce ( act ( anti g, g ) :: ( a -> a ) -> ( a -> a ) )

-- | Newtype for the action on a type through its 'Finitary' instance.
--
-- > data ABCD = A | B | C | D
-- >   deriving stock    ( Eq, Generic )
-- >   deriving anyclass Finitary
-- >   deriving ( Act ( Sum ( Finite 4 ) ), Torsor ( Sum ( Finite 4 ) ) )
-- >     via Finitely ABCD
--
-- Sizes are checked statically. For instance if we had instead written:
--
-- >   deriving ( Act ( Sum ( Finite 3 ) ), Torsor ( Sum ( Finite 3 ) ) )
-- >     via Finitely ABCD
--
-- we would have gotten the error messages:
--
-- > * No instance for (Act (Sum (Finite 3)) (Finite 4))
-- > * No instance for (Torsor (Sum (Finite 3)) (Finite 4))
--
newtype Finitely a = Finitely { getFinitely :: a }
  deriving stock   ( Show, Read, Data, Generic, Generic1 )
  deriving newtype ( Eq, Ord, NFData )

-- | Act on a type through its 'Finitary' instance.
instance ( Semigroup s, Act    s ( Finite n ), Finitary a, n ~ Cardinality a )
        => Act    s ( Finitely a ) where
  act s = Finitely . fromFinite . act s . toFinite . getFinitely
-- | Torsor for a type using its 'Finitary' instance.
instance ( Group     g, Torsor g ( Finite n ), Finitary a, n ~ Cardinality a )
      => Torsor g ( Finitely a ) where
  Finitely x --> Finitely y = toFinite x --> toFinite y

-----------------------------------------------------------------

-- | A left __torsor__ consists of a /free/ and /transitive/ left action of a group on an inhabited type.
--
-- This precisely means that for any two terms @x@, @y@, there exists a /unique/ group element @g@ taking @x@ to @y@,
-- which is denoted @ y <-- x @ (or @ x --> y @, but the left-pointing arrow is more natural when working with left actions).
--
-- That is @ y <-- x @ is the /unique/ element satisfying:
--
-- @( y <-- x ) • x = y@
--
--
-- Note the order of composition of @<--@ and @-->@ with respect to @<>@:
--
-- > ( z <-- y ) <> ( y <-- x ) = z <-- x
--
-- > ( y --> z ) <> ( x --> y ) = x --> z
class ( Group g, Act g x ) => Torsor g x where
  {-# MINIMAL (-->) | (<--) #-}
  -- | Unique group element effecting the given transition
  (<--), (-->) :: x -> x -> g
  (-->) = flip (<--)
  (<--) = flip (-->)

infix 7 -->
infix 7 <--

-- | Any group is a torsor under its own natural left action.
instance Group g => Torsor g g where
  h <-- g = h <> inverse g

instance Num a => Torsor ( Sum a ) a where
  (<--) = coerce ( (<--) :: Sum a -> Sum a -> Sum a )

-- | Given
-- 
--  * \( g \in G \) acting on \( A \),
--  * \( B \) a torsor under \( H \),
--  * a map \( p \colon A \to B \),
--
-- this function returns the unique element \( h \in H \) making the following diagram commute:
--
-- <<img/intertwiner.svg>>
intertwiner :: forall h g a b. ( Act g a, Torsor h b ) => g -> ( a -> b ) -> a -> h
intertwiner g p a = p a --> p ( g • a )
