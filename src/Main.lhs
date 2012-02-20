Main
====

Part of Augh!, the Anorexic UML to Graphviz compiler for Haskell
Licenced under the terms of the MIT Licence (see COPYING)

> module Main where
> import Augh.UmlTypes
> import Augh.Uml
> import Augh.Graphviz

Main program
------------

The generator can be used as a standalone program, taking in a
Read-able algebraic data-type representation of a UML diagram and
spitting out the Graphviz code.

> main = do
>        input <- getContents
>        let uml = (read input)::[Uml]
>        putStr (compile (transformUmlList "UML" uml))