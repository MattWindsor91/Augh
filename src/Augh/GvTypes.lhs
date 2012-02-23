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

***

Compass directions are used for ports.

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

***

> data Port = CompassPort CompassDirection
>           | IdAndCompassPort String CompassDirection
>           deriving (Show, Read)

***

A node ID can either refer to a named node, or a given port on a named
node.

> data NodeId = UnportedNodeId String
>             | PortedNodeId String Port
>             deriving (Show, Read)

***

A statement can be a node, an edge, a subgraph or a composition of
statements.

> data Stmt = Stmt :# Stmt
>           | NodeStmt NodeId AttrList
>           | EdgeStmt EdgeOperand EdgeRhs AttrList
>           | SubgraphStmt String Stmt
>           deriving (Show, Read)

***

A Graphviz graph can be an (undirected) graph or di(rected) graph.
For Augh!, we only use Digraph, but Graph is included for
completeness.

> data Graphviz = Graph String Stmt
>               | Digraph String Stmt
>               deriving (Show, Read)

Fixities
--------

> infixr 5 :#
> infixr 5 :&
> infixr 6 :=: