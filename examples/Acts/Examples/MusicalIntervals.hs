{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingVia
  , MultiParamTypeClasses
  , PatternSynonyms
  , TupleSections
  , TypeApplications
  , ViewPatterns
#-}

{-# OPTIONS_HADDOCK ignore-exports #-}

{-|
Module: Acts.MusicalIntervals

Illustrative usage of 'Group', 'Act' and 'Torsor': manipulation of musical intervals.


The musical distance between two musical notes is a musical interval.

Intervals can be compounded and inverted, so they form a 'Group'.

Notes can be translated by a given interval, which is an 'Act' of intervals on notes.

There's a unique musical interval taking any note to any other given one, so notes are a torsor under intervals.


This functionality is useful in providing enharmonically correct voicings of chords.
-}

module Acts.Examples.MusicalIntervals where

-- base
import Data.Monoid
  ( Sum(..) )
import GHC.Generics
  ( Generic )

-- finitary
import Data.Finitary
  ( Finitary )

-- finite-typelits
import Data.Finite
  ( Finite )

-- groups
import Data.Group
  ( Group(..) )

-- acts
import Data.Act
  ( Act(..), Torsor(..), Finitely(..) )

-----------------------------------------------------------------
-- * Musical notes
--
-- $notenames
-- We begin by defining note names, which are acted upon by the cyclic group of order 7.

-- | Cyclic group of order 7.
type C7 = Sum ( Finite 7 )

-- | Musical note names.
--
-- The enumeration starts with @C@ to conform with scientific pitch notation.
data NoteName = C | D | E | F | G | A | B
  deriving stock    ( Eq, Ord, Show, Enum, Bounded, Generic )
  deriving anyclass Finitary
  deriving ( Act C7, Torsor C7 )
    via Finitely NoteName

-- $deriving1
-- In this case we used @DerivingVia@ to derive the action of @C7@
-- through the 'Finitary' instance of 'NoteName' by using the 'Finitely' newtype.

-- | Alterations, i.e. sharps and flats.
--
-- Pattern synonyms such as 'Sharp' and 'Flat' are also bundled.
newtype Alteration = Alteration { getAlteration :: Int }
  deriving ( Semigroup, Monoid, Group )
    via Sum Int

-- $deriving2
-- Note the use of @DerivingVia@ to transfer algebraic operations from @Sum Int@.
-- 
-- For non-newtypes, one can use generics, for example:
--
-- > data Klein4 = Klein4 ( C 2 ) ( C 2 )
-- >   deriving stock Generic
-- >   deriving ( Semigroup, Monoid, Group )
-- >     via Generically Klein4
--
-- This uses the 'Generically' newtype from the @generic-data@ library.

-- | Note names such as @A4@ or @C#6@: note name, alteration, and octave (scientific pitch notation).
data Note = Note { name :: NoteName, alteration :: Alteration, octave :: Int }

-----------------------------------------------------------------
-- * Musical intervals
--
-- $intervals
-- An interval is represented as a number of scale steps to take (relative to the major scale),
-- together with an additional alteration to apply.
--
-- For instance, a major third is two steps up (diatonic steps relative to the root in a major scale):
--
-- > > Steps ( Sum 2 ) Natural
-- > major 3rd up
--
-- A minor sixth is 5 steps up, and then a flat:
--
-- > > Steps ( Sum 5 ) Flat
-- > minor 6th up
--
-- The smart constructor 'Interval' is also provided that is more intuitive to use:
--
-- > > Interval 3 Natural
-- > major 3rd up
--
-- > > Interval 7 Flat
-- > minor 7th up
--
-- Note that the @Semigroup@/@Group@ operations on intervals are __not__ the obvious ones, e.g.:
--
-- > > Steps ( Sum 2 ) Natural
-- > major 3rd up
--
-- > > Steps ( Sum (-2) ) Natural
-- > minor 3rd down
--
-- > > invert ( Steps ( Sum 2 ) Natural )
-- > Steps ( Sum (-2) ) Flat
-- > major 3rd down

-- | Musical interval: steps (relative to the root in a major scale) and additional alteration.
data Interval = Steps { intervalSteps :: Sum Int, intervalAlteration :: Alteration }

-- | Compute the number of semitones in an interval, using the reference of the C major scale.
semitones :: Interval -> Int
semitones ival = case act ival ( Note C Natural 0 ) of
  Note n a o -> 12 * o + getAlteration a + majorValue
    where
      majorValue = let i = fromEnum n in 2 * i - fromEnum ( i >= 3 )

-- $interval_operations
-- To define algebraic operations on intervals,
-- we use an equivariant bijection to the product group @( Sum Int, Sum Int )@.
--
-- Note that @( Sum Int, Sum Int )@ is automatically a 'Semigroup', 'Monoid' and 'Group'
-- using the product structure.

-- | Forward part of the bijection.
straighten :: Interval -> ( Sum Int, Sum Int )
straighten ival@( Steps steps _ ) = ( steps, Sum $ semitones ival )
-- | Back part of the bijection.
twist :: ( Sum Int, Sum Int ) -> Interval
twist ( i, Sum a ) = Steps i ( Alteration ( semitones ( Steps i mempty ) ) --> Alteration a )

instance Semigroup Interval where
  iv1 <> iv2 = twist ( straighten iv1 <> straighten iv2 )
instance Monoid Interval where
  mempty = Steps mempty mempty
instance Group Interval where
  invert = twist . invert . straighten

-- | Intervallically correct action of intervals on notes.
--
--  * minor third up from @C@: @Eb@
--  * minor third up from @A@: @C@.
instance Act Interval Note where
  act ( Steps ( Sum steps ) a ) ( Note C a' o ) = Note ( act ( fromIntegral r :: C7 ) C ) ( a <> a' ) ( q + o )
    where
      q, r :: Int
      ( q, r ) = steps `divMod` 7
  act ival note = act ( ival <> ( Note C Natural 0 --> note ) ) ( Note C Natural 0 )

-- | Computes the interval between two notes.
--
-- > > Note C Natural 5 --> Note A Natural 4
-- > minor 3rd down
--
-- > > Note E Flat 4 --> Note A Natural 5
-- > augmented 11th up
instance Torsor Interval Note where
  Note C a o --> Note n a' o' = Steps ( Sum $ fromEnum n + 7 * (o' - o) ) ( a --> a' )
  note1 --> note2 = ( Note C Natural 0 --> note1 :: Interval ) --> ( Note C Natural 0 --> note2 :: Interval )

-----------------------------------------------------------------
-- * Illustration of the functionality

-- ** Chords

-- | Major triad: major third, perfect fifth.
majorTriad :: [ Interval ]
majorTriad = [ mempty, Interval 3 Natural, Interval 5 Natural ]

-- | Diminished seventh chord: minor third, diminished fifth, diminished seventh.
diminished7th :: [ Interval ]
diminished7th = [ mempty, Interval 3 Flat, Interval 5 Flat, Interval 7 DoubleFlat ]

-- | Minor 11th chord (Kenny Barron voicing).
minor11th :: [ Interval ]
minor11th = [ mempty, Interval 5 Natural, Interval 9 Natural
            , Interval 10 Flat, Interval 14 Flat, Interval 18 Natural
            ]

-- $chords
-- Example chords:
--
-- > > majorTriad <&> ( • Note C Natural 4 )
-- > [C4,E4,G4]
-- 
-- > > diminished7th <&> ( • Note G Sharp 3 )
-- > [G#3,B3,D4,F4]
--
-- > > minor11th <&> ( • Note D Natural 3 )
-- > [D3,A3,E4,F4,C5,G5]

-- ** Scales

-- | Modes of C major.
mode :: NoteName -> [ Interval ]
mode root =
  map
    ( \ ( n, i ) -> Note root Natural 0 --> Note n Natural i )
    ( map ( , 0 ) [ root .. maxBound ] ++ map ( , 1 ) [ minBound .. root ] )

-- | Phrygian scale.
phrygian :: [ Interval ]
phrygian = mode E

-- | Lydian scale.
lydian :: [ Interval ]
lydian = mode F

-- | Whole tone scale.
wholeTone :: [ Interval ]
wholeTone = scanl (<>) mempty
  [ Interval 2 Natural, Interval 2 Natural, Interval 2 Natural, Interval 3 DoubleFlat, Interval 2 Natural ]

-- $scales
-- Example scales:
--
-- > > phrygian <&> ( • Note E Natural 3 )
-- > [E3,F3,G3,A3,B3,C4,D4,E4]
--
-- > > phrygian <&> ( • Note C Sharp 3 )
-- > [C#3,D3,E3,F#3,G#3,A3,B3,C#4]
--
-- > > lydian <&> ( • Note C Natural 4 )
-- > [C4,D4,E4,F#4,G4,A4,B4,C5]
--
-- > > wholeTone <&> ( • Note G Natural 5 )
-- > [G5,A5,B5,C#6,Eb6,F6]

---------------------------------------------------
-- * Helper code
-- $end
-- End of main example code.
--
-- Follows: helper code for reading/showing musical notes and intervals.

pattern Natural :: Alteration
pattern Natural = Alteration 0
pattern Flat :: Alteration
pattern Flat = Alteration (-1)
pattern DoubleFlat :: Alteration
pattern DoubleFlat = Alteration (-2)
pattern Sharp :: Alteration
pattern Sharp = Alteration 1
pattern DoubleSharp :: Alteration
pattern DoubleSharp = Alteration 2

instance Show Alteration where
  show ( Alteration i ) = replicate ( abs i ) accidental
    where
      accidental :: Char
      accidental
        | i >= 0 = '#'
        | otherwise = 'b'

instance Show Note where
  show ( Note n alt oct ) = show n <> show alt <> show oct

pattern Interval :: Int -> Alteration -> Interval
pattern Interval i a <-
  ( ( \ ( Steps ( Sum steps ) alt ) -> ( if steps >= 0 then steps + 1 else steps - 1, alt ) )
  -> (i, a)
  )
  where
    Interval i a = if i < 0 then Steps ( Sum (i+1) ) a else Steps ( Sum (i-1) ) a

instance Show Interval where
  show ival@( Steps ( Sum i ) _ )
    | i == 0 || i == 7 || i == -7
    , let
        ivalName = case compare i 0 of
          LT -> "octave down"
          GT -> "octave up"
          EQ -> "unison"
    = if quality ival == "perfect"
      then ivalName
      else quality ival <> " " <> ivalName
    | i < 0
    = quality ( invert ival ) <> " " <> showOrdinal (-i+1) <> " down"
    | otherwise
    = quality ival <> " " <> showOrdinal (i+1) <> " up"

quality :: Interval -> String
quality ( Steps ( Sum i ) ( Alteration a ) )
  | ( i `mod` 7 ) `elem` [ 0, 3, 4 ]
  = case a of
      0 -> "perfect"
      _ -> 
        if a > 0
        then multiplicity   a  <> "augmented"
        else multiplicity (-a) <> "diminished"
  | otherwise
  = case a of
      0    -> "major"
      (-1) -> "minor"
      _    -> 
        if a > 0
        then multiplicity   a    <> "augmented"
        else multiplicity (-a-1) <> "diminished"

showOrdinal :: Int -> String
showOrdinal i
  | i < 0
  = "-" <> showOrdinal ( abs i )
  | i `mod` 10 == 1 && i `mod` 100 /= 11
  = show i <> "st"
  | i `mod` 10 == 2 && i `mod` 100 /= 12
  = show i <> "nd"
  | i `mod` 10 == 3 && i `mod` 100 /= 13
  = show i <> "rd"
  | otherwise
  = show i <> "th"

multiplicity :: Int -> String
multiplicity 1 = ""
multiplicity 2 = "doubly "
multiplicity 3 = "triply "
multiplicity 4 = "quadruply "
multiplicity 5 = "quintuply "
multiplicity 6 = "sextuply "
multiplicity 7 = "heptuply "
multiplicity n = show n <> "-tuply "
