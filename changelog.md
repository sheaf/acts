# Changelog for package `acts`

## 0.2.0.0 ( February 14, 2020 )

* Remove definition of cyclic groups.
It is instead suggested to use a library which defines modular arithmetic.    
For instance: `type C (n :: Nat) = Sum ( Finite n )`, using the `finite-typelits` library.

* `CyclicEnum` newtype changed to `Finitely` newtype, which uses `Finitary` instead of `Bounded + Enum`.
This ensures that the action is by a semigroup of the right cardinality.

* Remove `Act` instances for `Max`, `Min` to avoid possible overlap with user defined instances.

* Add `anti :: Group g => g -> Dual g` function to construct elements in the opposite _group_.    
Obsoletes the `Act` instance for `Dual` (now removed).

* Address a limitation of GHC < 8.10 with `DerivingVia` and `MultiParamTypeClasses`,
by manually writing some instances.

## 0.1.0.0 ( February 13, 2020 )

* Initial release.
