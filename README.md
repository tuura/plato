# Plato

[![Build Status](https://travis-ci.org/tuura/plato.svg?branch=master)](https://travis-ci.org/tuura/plato)
[![Build status](https://ci.appveyor.com/api/projects/status/eyfuyi0v9v1ulcgn?svg=true)](https://ci.appveyor.com/project/jrbeaumont/plato-9uatd)

A DSL for asynchronous behavioural concept circuits specification, including a tool to translate concepts to Signal Transition Graphs
and Finite State Machines

### Requirements

To build and run the translation tool, Stack is needed, and can be downloaded for all operating systems from
<https://docs.haskellstack.org/en/stable/install_and_upgrade/>

### Build

	stack setup --no-system-ghc
	stack build

### Test

	stack test

### Translate to STG

Invoke the translate executable with a concept circuit file:

	stack runghc translate/Main.hs -- examples/Celement_with_env_1.hs

### Translate to FSM

Invoke the translate executable with a concept circuit file, including the "-f" flag:

	stack runghc translate/Main.hs -- examples/Celement_with_env_1.hs -f

### Manual

The manual can be found at [doc/manual.md](https://github.com/tuura/plato/blob/master/doc/manual.md)
