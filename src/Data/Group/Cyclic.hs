{-# LANGUAGE
    DataKinds
  , DeriveDataTypeable
  , DeriveGeneric
  , DerivingVia
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , PatternSynonyms
  , PolyKinds
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , ViewPatterns
#-}

{-|
Module: Data.Group.Cyclic

Cyclic groups: integers modulo @n@ (clock arithmetic).
-}

module Data.Group.Cyclic
  ( Cyclic(Cyclic), getCyclic
  , C, Z
  , CyclicEnum(..)
  , pattern Involution, involution
  , rootOfUnity
  )
  where

-- base
import Data.Coerce
  ( coerce )
import Data.Complex
  ( Complex(..), conjugate, mkPolar )
import Data.Data
  ( Data )
import Data.Monoid
  ( Sum(..), Product(..) )
import Data.Proxy
  ( Proxy(..) )
import GHC.Generics
  ( Generic, Generic1 )
import GHC.TypeNats
  ( Nat, KnownNat, natVal
  , type (<=)
  )

-- deepseq
import Control.DeepSeq
  ( NFData )

-- finite-typelits
import Data.Finite
  ( Finite, getFinite )

-- acts
import Data.Act
  ( Act(..), Torsor(..) )
import Data.Group
  ( Group(..) )

-----------------------------------------------------------------

-- | Cyclic group of order @n@: integers with addition modulo @n@.
newtype Cyclic n = MkCyclic { runCyclic :: Finite n }
  deriving stock   ( Show, Generic, Generic1 )
  deriving newtype ( Eq, Ord, Enum, Bounded, NFData )
deriving via ( Sum ( Finite n ) ) instance KnownNat n => Semigroup ( Cyclic n ) 
deriving via ( Sum ( Finite n ) ) instance ( KnownNat n, 1 <= n ) => Monoid ( Cyclic n )
deriving via ( Sum ( Finite n ) ) instance ( KnownNat n, 1 <= n ) => Group  ( Cyclic n )

{-# COMPLETE Cyclic #-}
-- | Smart pattern and constructor for elements of cyclic groups.
pattern Cyclic :: forall n. KnownNat n => Int -> Cyclic n
pattern Cyclic i <- ( fromIntegral . getFinite . runCyclic -> i )
  where
    Cyclic i = MkCyclic ( fromIntegral ( i `mod` ( fromIntegral ( natVal ( Proxy @n ) ) ) ) )

-- | Obtain a representative in the range \( [0, n[ \).
getCyclic :: forall n. KnownNat n => Cyclic n -> Int
getCyclic ( Cyclic i ) = i

-- | Synonym for finite cyclic group.
type C ( n :: Nat ) = Cyclic n
-- | Synonym for infinite cyclic group.
type Z = Sum Int

instance KnownNat n => Act ( Cyclic n ) Int where
  act ( Cyclic f ) j
    | r + i >= n
    = ( i - n ) + j
    | otherwise
    = i + j
    where
      i, n, r :: Int
      i = fromIntegral f
      n = fromIntegral ( natVal ( Proxy @n ) )
      r = j `mod` n

-- | Nontrivial element of cyclic group of order 2.
pattern Involution :: Cyclic 2
pattern Involution = Cyclic 1

-- | Act by an involution.
involution :: Act ( Cyclic 2 ) x => x -> x
involution = act Involution

instance Act ( Cyclic 2 ) Bool where
  act Involution = not
  act _          = id

instance Num i => Act ( Cyclic 2 ) ( Sum i ) where
  act Involution = coerce ( negate :: i -> i )
  act _          = id

instance Fractional i => Act ( Cyclic 2 ) ( Product i ) where
  act Involution = coerce ( recip :: i -> i )
  act _          = id

instance Num a => Act ( Cyclic 2 ) ( Complex a ) where
  act Involution = conjugate
  act _          = id

-- | Natural complex representations of finite cyclic groups as roots of unity.
rootOfUnity :: forall a n. ( KnownNat n, Floating a ) => Cyclic n -> Complex a
rootOfUnity ( Cyclic f ) = mkPolar 1 ( 2 * pi * i / n )
  where
    i, n :: a
    i = fromIntegral f
    n = fromIntegral ( natVal ( Proxy @n ) )

-- | Newtype for cycling through elements in a finite enumeration.
--
-- > data ABCD = A | B | C | D 
-- >   deriving stock ( Enum, Bounded )
-- >   deriving ( Act ( Cyclic 4 ), Torsor ( Cyclic 4 ) )
-- >     via CyclicEnum ABCD
-- 
-- > > act ( Cyclic 2 ) C
-- > A
--
-- > > act ( Cyclic (-1) ) A
-- > D
--
-- > > ( C --> B :: Cyclic 4 )
-- > Cyclic 3
--
-- __Warning__
-- It is unfortunately not checked that the size of the group
-- matches the size of the finite enumeration.
-- Please manually ensure this condition.
newtype CyclicEnum a = CyclicEnum { getCyclicEnum :: a }
  deriving stock   ( Show, Data, Generic, Generic1 )
  deriving newtype ( Eq, Ord, Enum, Bounded, NFData )

instance ( Enum a, Bounded a, KnownNat n ) => Act ( Cyclic n ) ( CyclicEnum a ) where
  act ( Cyclic f ) a = toEnum j
    where
      b_min, b_max, i, j :: Int
      b_min = fromEnum ( minBound @a )
      b_max = fromEnum ( maxBound @a )
      i = fromIntegral f
      j = b_min + ( fromEnum a + i - b_min ) `mod` ( 1 + b_max - b_min )
      -- Assumes n ~ ( 1 + b_max - b_min ).
instance ( Enum a, Bounded a, KnownNat n, 1 <= n ) => Torsor ( Cyclic n ) ( CyclicEnum a ) where
  a --> b = Cyclic . fromIntegral . ( `mod` n ) $ fromEnum b - fromEnum a
    where
      n :: Int
      n = fromIntegral ( natVal ( Proxy @n ) )
