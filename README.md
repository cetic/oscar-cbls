# OscaR.cbls: An open source library for Constraint-Based Local Search

OscaR.cbls is an open source library that proposes a generic API to define a Constraint-Based Local Search (CBLS)
solver.

Check our company website for more info about OscaR.cbls:
https://asset.cetic.be/en/oscar/

## Getting started

In scala, add this in your `build.sbt` file:

```scala
libraryDependencies += "oscar" %% "oscar-cbls_2.13" % "X.Y.Z"
``` 

See the `src/main/scala/oscar/cbls/examples` folder to see examples of using OscaR to build problem-solving engines.

A more complete documentation can be found [here](https://oscarlib.readthedocs.io/en/latest/). An important refactoring
is being made for the upcoming version 6.0.0 of OscaR, and as such this documentation is not yet up-to-date. However, it
still presents the key concepts and may be useful to understand the way OscaR is meant to be used.

## Build

OscaR.cbls was developed in Scala 2.13.14 (check `build.sbt`), built with SBT 1.9.9
(check `project/built.properties`) and run with Java 21.

You can use other combinations of compatible versions as mentioned in
https://docs.scala-lang.org/overviews/jdk-compatibility/overview.html

## Local Search in a nutshell

Local search is a class of heuristic methods that are used to solve hard optimization problems. In an optimization
problem, we want to find a **solution** that optimizes (minimizes or maximizes) a given **objective function**, subject
to certain constraints. The process of solving a problem with local search starts with an arbitrary initial solution,
which is then slightly modified to get a *neighboring solution* that improves the objective function. Exploring the
neighboring solutions is done through a **neighborhood**.

A key concept of constraint-based local search is to use efficient algorithms to incrementally update the value of the
objective function when a neighborhood is explored; these algorithms are then embedded in the constraint representations
as well as in **invariants**, which are objects used to represent the dependencies between variables.

## Key features

* 3 types of variables:
    * **IntVariable** (an integer)
    * **SetVariable** (a set of integers)
    * **SeqVariable** (a sequence of integers)
* Most common **constraints** for integers: Sum or Min/Max of an array, Sum, Mult, etc.
* Most common **constraints** for set: Cardinality, Intersection, Union, etc.
* Most common **neighborhoods**: Assign a value to a variable, swap two values, etc.
* Rich library for routing problems:
    * Most common **neighborhoods**: One point move, two-opt, three-opt, etc.
    * Most common **constraints** for routing: length of a route, etc.
* Rich API for problem definition
* Rich API for **neighborhood** and search procedure definition

## Contributing

If you want to contribute, you can create a pull request, which the team will review.

## Support

If you have any question, bug or feature request, you can add an issue about it.

**NB:** Soon available on Maven.