Main
====

Part of Augh!, the Anorexic UML to Graphviz compiler for Haskell
Licenced under the terms of the MIT Licence (see COPYING)

> module Main where
> import Augh.Uml ( transformUmlList )
> import Language.Dot.Pretty ( renderDot )

Main program
------------

The generator can be used as a standalone program, taking in a
Read-able algebraic data-type representation of a UML diagram and
spitting out the Graphviz code.

> main :: IO ()
> main = interact ( renderDot . transformUmlList "UML" . read )

