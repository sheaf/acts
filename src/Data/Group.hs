{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveDataTypeable
  , DeriveGeneric
  , DerivingVia
  , GeneralizedNewtypeDeriving
  , KindSignatures
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , TypeOperators
  , UndecidableInstances
#-}

{-|
Module: Data.Group

A 'Group' is a 'Monoid' for which the monoid operation can be undone.

That is, \( G \) is a group if each \( g \in G \) has an inverse element \( g^{ -1 } \) such that

\[ g^{ -1 } < \! > g = \text{mempty} = g < \! > g^{ -1 } \]

Such inverses are necessarily unique.


In Haskell, groups are mostly useful to describe objects possessing certain symmetries (such as translation or rotation).

To automatically derive 'Group' instances, you can:

- Use @DerivingVia@ to coerce an existing instance:

> > newtype Seconds   = Seconds { getSeconds :: Double }
> > newtype TimeDelta = TimeDelta { timeDeltaInSeconds :: Seconds }
> >   deriving ( Semigroup, Monoid, Group )
> >     via Sum Double

- Use 'Generic' and 'Generic.Data.Generically':

> > data MyRecord
> >   = MyRecord
> >   { field1 :: Sum Double
> >   , field2 :: Product Double
> >   , field3 :: Ap [] ( Sum Int, Sum Int )
> >   }
> >   deriving Generic
> >   deriving ( Semigroup, Monoid, Group )
> >     via Generically MyRecord
-}


module Data.Group
  ( Group(..)
  , Isom(..)
  )
  where

-- base
import Control.Monad.ST
  ( ST )
import Data.Data
  ( Data )
import Data.Functor.Const
  ( Const(..) )
import Data.Functor.Contravariant
  ( Op(..) )
import Data.Functor.Identity
  ( Identity(..) )
import Data.Monoid
  ( Ap(..), Sum(..), Product(..) )
import Data.Ord
  ( Down(..) )
import Data.Semigroup
  ( Semigroup(..), Dual(..) )
import Data.Proxy
  ( Proxy(..) )
import GHC.Generics
  ( Generic, Generic1
  , U1(..), Rec1(..), M1(..), K1(..), Par1(..), (:*:)(..)
  , V1, (:+:)
  )
import qualified GHC.Generics as Generic
  ( Generic(..) )
import GHC.TypeLits
  ( TypeError, ErrorMessage(Text) )

-- deepseq
import Control.DeepSeq
  ( NFData )

-- generic-data
import Generic.Data
  ( Generically(..) )

-----------------------------------------------------------------------

-- | A 'Group' is a 'Monoid' with inverses:
--
-- * @ inverse g <> g = g <> inverse g = mempty @
--
-- * @ inverse (g <> h) = inverse h <> inverse g @
class Monoid g => Group g where
  {-# MINIMAL inverse | gtimes #-}
  -- | Group inversion anti-homomorphism.
  inverse :: g -> g
  inverse = gtimes ( (-1) :: Int )

  -- | Take the @n@-th power of an element.
  gtimes :: Integral n => n -> g -> g
  gtimes n = case compare n 0 of
    EQ -> const mempty
    GT -> stimes n
    LT -> stimes ( negate n ) . inverse

-----------------------------------------------------------------------
-- Instances.

-- | Trivial group.
instance Group () where
  inverse  _ = ()
  gtimes _ _ = ()

-- | Additive groups (via 'Num').
instance Num a => Group ( Sum a ) where
  inverse  ( Sum a ) = Sum ( negate a )
  gtimes n ( Sum a ) = Sum ( fromIntegral n * a )

-- | Multiplicative group (via 'Num').
instance Fractional a => Group ( Product a ) where
  inverse  ( Product a ) = Product ( recip a )
  gtimes n ( Product a ) = Product ( a ^^ toInteger n )

-- | Opposite group.
instance Group a => Group ( Dual a ) where
  inverse  ( Dual a ) = Dual ( inverse a )
  gtimes n ( Dual a ) = Dual ( gtimes n a )

-- | Lifting group operations through an applicative functor.
instance ( Group a, Applicative f ) => Group ( Ap f a ) where
  inverse  = fmap inverse
  gtimes n = fmap ( gtimes n )

deriving via Ap ((->) r) a instance Group a => Group ( r  ->  a )
deriving via Ap IO       a instance Group a => Group ( IO     a )
deriving via Ap (ST s)   a instance Group a => Group ( ST s   a )

deriving newtype instance Group a => Group ( Down     a )
deriving newtype instance Group a => Group ( Identity a )
deriving newtype instance Group a => Group ( Const a b )
deriving newtype instance Group a => Group ( Op    a b )

instance Group ( Proxy p ) where
  inverse  _ = Proxy
  gtimes _ _ = Proxy

instance ( Group g1, Group g2 )
      => Group ( g1, g2 ) where
  inverse  ( g1, g2 ) =
    ( inverse g1, inverse g2 )
  gtimes n ( g1, g2 ) =
    ( gtimes n g1, gtimes n g2 )

instance ( Group g1, Group g2, Group g3 )
      => Group ( g1, g2, g3 ) where
  inverse  ( g1, g2, g3 ) =
    ( inverse g1, inverse g2, inverse g3 )
  gtimes n ( g1, g2, g3 ) =
    ( gtimes n g1, gtimes n g2, gtimes n g3 )

instance ( Group g1, Group g2, Group g3, Group g4 )
      => Group ( g1, g2, g3, g4 ) where
  inverse  ( g1, g2, g3, g4 ) =
    ( inverse g1, inverse g2, inverse g3, inverse g4 )
  gtimes n ( g1, g2, g3, g4 ) =
    ( gtimes n g1, gtimes n g2, gtimes n g3, gtimes n g4 )

instance ( Group g1, Group g2, Group g3, Group g4, Group g5 )
      => Group ( g1, g2, g3, g4, g5 ) where
  inverse  ( g1, g2, g3, g4, g5 ) =
    ( inverse g1, inverse g2, inverse g3, inverse g4, inverse g5 )
  gtimes n ( g1, g2, g3, g4, g5 ) =
    ( gtimes n g1, gtimes n g2, gtimes n g3, gtimes n g4, gtimes n g5 )

infix 7 :|:
-- | Data type to keep track of a pair of inverse elements.
data Isom a = (:|:) { to :: a, from :: Dual a }
  deriving stock    ( Show, Read, Data, Generic, Generic1 )
  deriving anyclass NFData
instance Semigroup a => Semigroup ( Isom a ) where
  ( p1 :|: q1 ) <> ( p2 :|: q2 ) = ( p1 <> p2 ) :|: ( q1 <> q2 )
instance Monoid a => Monoid ( Isom a ) where
  mempty = mempty :|: mempty
instance Monoid a => Group ( Isom a ) where
  inverse ( p :|: q ) = getDual q :|: Dual p

-- Generics.

instance Group ( U1 p ) where
  inverse  _ = U1
  gtimes _ _ = U1

deriving newtype instance Group ( f p ) => Group ( Rec1 f p )
deriving newtype instance Group ( f p ) => Group ( M1 i c f p )
deriving newtype instance Group g       => Group ( K1 i g p )
deriving newtype instance Group g       => Group ( Par1 g )

instance ( Group ( f1 p ), Group ( f2 p ) ) => Group ( (f1 :*: f2) p ) where
  inverse  ( g1 :*: g2 ) = ( inverse  g1 :*: inverse  g2 )
  gtimes n ( g1 :*: g2 ) = ( gtimes n g1 :*: gtimes n g2 )

instance
  ( Generic g
  , Monoid  ( Generic.Rep g () )
  , GGroup  ( Generic.Rep g )
  )
  => Group ( Generically g ) where
  inverse  = Generically . Generic.to . ginverse  . Generic.from . unGenerically
  gtimes n = Generically . Generic.to . ggtimes n . Generic.from . unGenerically

-- | Type class used for deriving 'Group' instances generically.
class GGroup f where
  ginverse :: f p -> f p
  ggtimes  :: Integral n => n -> f p -> f p

instance GGroup U1 where
  ginverse  _ = U1
  ggtimes _ _ = U1

deriving newtype instance GGroup f => GGroup ( Rec1 f )
deriving newtype instance GGroup f => GGroup ( M1 i c f )

instance Group g => GGroup ( K1 i g ) where
  ginverse  ( K1 g ) = K1 ( inverse  g )
  ggtimes n ( K1 g ) = K1 ( gtimes n g )

instance ( GGroup f1, GGroup f2 ) => GGroup ( f1 :*: f2 ) where
  ginverse  ( g1 :*: g2 ) = ( ginverse  g1 :*: ginverse  g2 )
  ggtimes n ( g1 :*: g2 ) = ( ggtimes n g1 :*: ggtimes n g2 )

instance
  TypeError ( 'Text "No 'Group' instance for empty generic representation." )
  => GGroup V1 where
  ginverse  _ = error "unreachable"
  ggtimes _ _ = error "unreachable"

instance
  TypeError ( 'Text "No 'Group' instance for generic sum type." )
  => GGroup ( f1 :+: f2 ) where
  ginverse  _ = error "unreachable"
  ggtimes _ _ = error "unreachable"
