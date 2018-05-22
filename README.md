[![Hackage](https://img.shields.io/hackage/v/data-diverse.svg)](https://hackage.haskell.org/package/data-diverse)
[![Build Status](https://secure.travis-ci.org/louispan/data-diverse.png?branch=master)](http://travis-ci.org/louispan/data-diverse)

"Data.Diverse.Many" is an extensible record for any size encoded efficiently as (Seq Any).

"Data.Diverse.Which" polymorphic variant of possibilities encoded as (Int, Any).

Provides getters, setters, projection, injection, folds, and catamorphisms;
accessed by type or index or label.

Refer to [ManySpec.hs](https://github.com/louispan/data-diverse/blob/master/test/Data/Diverse/ManySpec.hs) and [WhichSpec.hs](https://github.com/louispan/data-diverse/blob/master/test/Data/Diverse/WhichSpec.hs) for example usages.

Iso, Lens and Prisms are provided in [data-diverse-lens](http://hackage.haskell.org/package/data-diverse-lens)

# Changelog

* 4.0.0.0
  - Renamed `prefix` to `consMany` and `postfix` to `snocMany`.

* 3.1.0.0
  - Removed `Read` instance for `Which []` since it is uninhabitable.
  - `xxxTag` functions only rely on `UniqueMember`, not `UniqueLabelMember`

* 3.0.0.0
  - Renamed `fetch` to `grab` to avoid conflicting with Haxl.
  - Removed unused type functions from removed splitting functions (Before,To,After,From,Length)
  - Added `impossible'` and ability to diversify and reinterpret `Which '[Void]`
  - Added `zilch`
  - Removed `CanAppendUnique` (not useful).

* 2.0.1.0
  - Simplified type synonyms for Which. Added Reinterpreted constraint synonym.

* 2.0.0.0
  - Breaking change: the prime (xxx') version of functions are now consistently the simpler or non-polymorphic version.
    - This is more consistent with `Control.Lens` as well.
    - This means the following are swapped:
      - `replace`, `replace'`
      - `replaceL`, `replaceL'`
      - `replaceTag`, `replaceTag'`
      - `replaceN`, `replaceN'`
      - `amend`, `amend'`
      - `amendL`, `amendL'`
      - `amendN`, `amendN'`
  - Breaking change: Removed proxy argument from `fetchL/Tag/N`, `replaceXXX`, `selectL/Tag/N`, `amendXXX`, `pickL/Tag/N`, `trialL/Tag/N`
    relying soley on `TypeApplications` and now requiring `AllowAmbiguousTypes`.
    The `Proxy` is also removed from the Read/Show serialized format.
  - Rearranged type variables in `fetchL/N`, `replaceL/Tag/N`, `pickL/Tag/N`, `trialL/Tag/N` type parameters,
    so the type variable ordering is consistently label, orig to change, smaller to larger, ie. `l/n`, `x`, `y`, `xs`

* 1.3.0.0
  - Removed splitting operations added in 1.2.0.0
  - added `xxxTag` version of label operations that also automatically untags the field.

* 1.2.0.3
  - `PolyKinds` for `Which`
  - Removed cabal upper bounds

* 1.2.0.2
  - Added `insert`/`remove` for GHC < 8.2
  - Removed type functions `UniqueMemberAt`, `MaybeUniqueMemberAt`

* 1.2.0.1
  - `insert`/`remove` is not available in GHC 8.2 onwards.

* 1.2.0.0
  - Rerranged type variable for xxxL and xxxN functions so that the
    `x` inferrred from label `l` or index `n` is after `proxy`.
    - This affects `fetch[L|N]`, `replace[L|N]`, `replace[L|N]'`, `pick[L|N]`
  - Depends on at least containers-0.5.8.2 for `Data.Sequence.insertAt`
  - Added splitting operations: `split[Before|After][|L|N]`, `inset[Before|After][|L|N]`,
    `insert[Before|After][|L|N]`, `remove[Before|After][|L|N]`
  - Renamed type function `Without` to `Remove` to be consistent with new `remove` method.

* 1.1.0.0
  - Added `CaseFunc` and `CaseFunc'` which replaces `CaseTypeable` (eg `CaseFunc @Typeable`)
    <https://github.com/louispan/data-diverse/issues/6>
  - Replaced `IsAll` constraint with `AllConstrained`.

* 1.0.0.1
  - Added `CaseTypeable'` as an example of polymorphic `Case` that doesn't change the type.

* 1.0.0.0
  - The exposed api shouldn't break, but there are a lot of internal changes.
  - Added `AFunctor` which can map over the types in the 'Many'
    <https://github.com/louispan/data-diverse/issues/5>.
  - Added friendlier type synomyns `Collect` and `CollectN` for `collect` and `collectN`
  - Expose type of 'Collector' and 'CollectorN'
  - Replace type parameter `r` from `Case` typeclass with `CaseResult` type family.
  - Replaced `CasesResult` type function with `IsAll` and `CasesResults` type functions.
  - All `CaseXxx` type variables now end with r xs.
  - All `CaseXxxN` type variables now end with r n xs.

* 0.11.0.0
  - Added `impossible` modelled after `Data.Void.absurd`
    <https://github.com/louispan/data-diverse/issues/4>
  - Removed `zilch` so `Which '[]` is uninhabited like `Data.Void.Void`, making 'impossible' safe to use.
  - Removed `Monoid` and changed `Show`, `Read` and `Generic` instances for `Which '[]` to be partial
    just like Data.Void.Void.
  - Added `instance Reduce (Which '[]) (Switcher c '[] r)`, which follows from `impossible`.

* 0.10.0.0
  - Renamed `Switch` to `Switcher`. Switch is now a type synonym for `switch` constraints
  - Added CasesResult type family to help infer the result of `cases`
  - Added Semigroup and Monoid instances for all Many xs.
  - Added Maybe versions of trial, and reinterpret
  - Renamed `reinterpetN` to `reinterpretN'`
  - Renamed `impossible` to `zilch`.
  - Allowed `reintepret`ing and `diversify`ing `zilch` to `zilch`
  - Removed zipped type variable from `Amend` constraints.
  - Removed r type variable from `Reduce` typeclass.
  - Rearranged type variables in `fetch`, `replace`, `pick`, `trial`, `Diversify` type parameters,
    so the type variable ordering is consistently smaller to larger, ie. `x`, `xs`, `branch`, `tree`
  - Added `diversify'` for allowing rearranging the types only.

* 0.9.0.1
  - Fixed GHC 8.2.1 test failure due to changed TypeRep show instance.

* 0.9.0.0
  - Breaking changes: Renamed Many.sliceL/R to Many.viewf/b
  - Renamed TypeLevel.Internal.MissingImpl to IsUniqueImpl.
  - Added postifx' with SnocUnique and append' with AppendUnique.
  - Added Semigroup & Monoid instances for `Many '[]` and `Which '[]`
  - Fixed GHC 8.2 compile error with importing GHC.Prim (Any)

* 0.8.1.0
  - Added NFData instance for Which.
  - Forgot to expose Many.sliceL and Many.sliceR.

* 0.8.0.0
  - Changed internal representation to (Data.Seq Any) for a further 2x append speedup.
  - Added NFData instance for Many.

* 0.7.0.0
  - Removed NOINLINE pragmas.
  - Changed internal representation to (Int, Data.IntMap Any) for a 2.5x append speedup.

* 0.6.0.0
  - Moved lens to data-diverse-lens

* 0.5.0.0
  - Renamed type level functions module from Type to TypeLevel

* 0.4.0.0
  - Removed Emit typeclass, breaking renames. Added label accessors.

* 0.1.0.0
  - Initial version represented as (Int, Data.Map Int Any)
