# Changelog

This file documents changes in the OscaR.cbls project.

## [6.0.0] - 2026-03-26

Refactoring of the OscaR.cbls library.

### Refactor

- Refactoring the core of OscaR.cbls:
  - Propagation structure
  - Main variable types:
	- IntVariable: Variable type that contains integer
	- SetVariable: Variable type that contains set of integers
	- SeqVariable: Variable type that contains an ordered sequence of integer
  - Abstract constraint structure
  - Abstract neighborhood structure
  - Abstract objective structure
- Refactoring the main invariants based on the abstract constraint structure
  - Main integer invariants (e.g. sum of an array of IntVariable, min/max of an array of IntVariable, basic operations on IntVariables (sum, minus, prod and div), etc.)
  - Main set invariants: (e.g. cardinality of a SetVariable, intersection, union and difference of SetVariable, etc.)
  - Main seq invariants: (e.g. content of a SeqVariable, size of a SeqVariable, etc.)
  - Main routing invariants (e.g. length of a route, number of nodes in a route, time windows, etc.) + Abstract structure for symbolic global constraint
- Implementation of a generic test bench for invariants
- Refactoring neighborhoods:
  - Main neighborhoods on IntVariable arrays (e.g. assign value, swap values, etc.)
  - Main routing neighborhoods (e.g. moving one point, two opt, three opt, insert nodes, etc.)
- Refactoring combinators for metaheuristics (e.g. best neighborhood first, cartesian products of neighborhoods, restarts, etc.)
- Refactoring multi-thread optimization (BETA) with distributed search heuristics (e.g. distributed modulo)
- Refactoring the api of the library
- Refactoring visualization primitives to help modeling problems
