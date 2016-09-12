# Concepts

[![Build Status](https://travis-ci.org/tuura/concepts.svg?branch=master)](https://travis-ci.org/tuura/concepts)
[![Build status](https://ci.appveyor.com/api/projects/status/dn6igqdxf3cq2t8w/branch/master?svg=true)](https://ci.appveyor.com/project/snowleopard/concepts/branch/master)

A DSL for asynchronous circuits specification.

### Build

	stack setup
	stack build

### Test

	stack test

### Translate to STG

Invoke the translate executable with a concept circuit file:

	stack runghc translate/Main.hs examples/celement_with_env_1.hs
	
### Manual

The manual can be found at [doc/manual.md](https://github.com/tuura/concepts/blob/master/doc/manual.md)
