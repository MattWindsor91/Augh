Augh.GvTypes
=============

Part of Augh!, the Anorexic UML to Graphviz compiler for Haskell
Licenced under the terms of the MIT Licence (see COPYING)

This module contains the algebraic data types for the Graphviz Dot
language, or (at least) the subset of which is usable in Augh!.  It is
derived from the language BNF at
http://www.graphviz.org/content/dot-language.

> module Augh.GvTypes where

> data EdgeOperand = EdgeNodeId String
>                  | Subgraph String Stmt
>                  deriving (Show, Read)

Graphviz supports LHS-only attribute assignments where true is
implied, but we'll handle that special case if and when the RHS of a
:=: statement is "true".

> data Attr = String :=: String
>           deriving (Show, Read)

> data EdgeRhs = CompoundEdgeRhs EdgeRhs EdgeRhs
>              | DirectedEdge EdgeOperand
>              deriving (Show, Read)

> data AttrList = Attrs [Attr]
>               | AttrList :& AttrList
>               deriving (Show, Read)

> data CompassDirection = North
>                       | Northeast
>                       | East
>                       | Southeast
>                       | South
>                       | Southwest
>                       | West
>                       | Northwest
>                       | Centre
>                       | Underscore
>                       deriving (Show, Read)

> data Port = CompassPort CompassDirection
>           | IdAndCompassPort String CompassDirection
>           deriving (Show, Read)

> data NodeId = UnportedNodeId String
>             | PortedNodeId String Port
>             deriving (Show, Read)

> data Stmt = NullStmt
>           | Stmt :# Stmt
>           | NodeStmt NodeId AttrList
>           | EdgeStmt EdgeOperand EdgeRhs AttrList
>           | SubgraphStmt String Stmt
>           deriving (Show, Read)

> data Graphviz = Graph String Stmt
>               | Digraph String Stmt
>               deriving (Show, Read)

Fixities
--------

> infixr 5 :#
> infixr 5 :&
> infixr 6 :=: