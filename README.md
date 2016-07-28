# Concepts

[![Build Status](https://travis-ci.org/tuura/concepts.svg?branch=master)](https://travis-ci.org/tuura/concepts)
[![Build status](https://ci.appveyor.com/api/projects/status/dn6igqdxf3cq2t8w/branch/master?svg=true)](https://ci.appveyor.com/project/snowleopard/concepts/branch/master)

A DSL for asynchronous circuits specification.

### Build

	cabal build

### Test

	cabal test --show-details=always

### Translate to STG

You first need `hint` and `concepts` installed:

	cabal install hint
	cabal install

Then invoke the translate executable with a concept circuit file:

	runghc translate/Main.hs examples/celement-concept-concat.hs
