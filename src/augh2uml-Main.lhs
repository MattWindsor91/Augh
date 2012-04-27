Main
====

Part of Augh!, the Anorexic UML to Graphviz compiler for Haskell
Licenced under the terms of the MIT Licence (see COPYING)

> module Main ( main ) where
> import Augh.Parser ( runParser )

Main program
------------

This stub reads in a pretty Augh file and converts it to the UML
abstract syntax.

> main :: IO ()
> main = interact ( either ( error . show ) show . runParser )

