[![Hackage](https://img.shields.io/hackage/v/data-diverse.svg)](https://hackage.haskell.org/package/data-diverse)
[![Build Status](https://secure.travis-ci.org/louispan/data-diverse.png?branch=master)](http://travis-ci.org/louispan/data-diverse)

"Data.Diverse.Many" is an extensible record for any size encoded efficiently as (Int, Map Int Any).

"Data.Diverse.Which" polymorphic variant of possibilities encoded as (Int, Any).

Provides getters, setters, projection, injection, folds, and catamorphisms;
accessed by type or index or label.

Refer to [ManySpec.hs](https://github.com/louispan/data-diverse/blob/master/test/Data/Diverse/ManySpec.hs) and [WhichSpec.hs](https://github.com/louispan/data-diverse/blob/master/test/Data/Diverse/WhichSpec.hs) for example usages.