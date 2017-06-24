[![Hackage](https://img.shields.io/hackage/v/data-diverse.svg)](https://hackage.haskell.org/package/data-diverse)
[![Build Status](https://secure.travis-ci.org/louispan/data-diverse.png?branch=master)](http://travis-ci.org/louispan/data-diverse)

Polymorhpic variant of possibilities encoded as (Int, Any).

Reasonably efficient extensible record for any size encoded as (Int, Map Int Any).

Provides getter/setter/projection/injection/fold/catamorphism functions,
accessed by type or index.

Refer to [WhichSpec.hs](test/Data/Diverse/WhichSpec.hs) and [ManySpec.hs](test/Data/Diverse/ManySpec.hs) for example usages.
