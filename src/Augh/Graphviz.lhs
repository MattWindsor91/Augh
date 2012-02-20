Augh.Graphviz
=============

Part of Augh!, the Anorexic UML to Graphviz compiler for Haskell
Licenced under the terms of the MIT Licence (see COPYING)

This module contains the algebraic data types for the Graphviz Dot
language, as well as functions converting from said ADTs into strings
of concrete Dot code.

> module Augh.Graphviz where

> data EdgeOperand = EdgeNodeId String
>                  | Subgraph String Stmt
>                  deriving Show

> data Attr = Attr :@ Attr
>           | TruthAssignment String
>           | String :=: String
>           deriving Show

> data EdgeRhs = CompoundEdgeRhs EdgeRhs EdgeRhs
>              | DirectedEdge EdgeOperand
>              deriving Show

> data AttrList = NullAttrList
>               | AttrSet Attr
>               | AttrList :& AttrList
>               deriving Show

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
>                       deriving Show

> data Port = CompassPort CompassDirection
>           | IdAndCompassPort String CompassDirection
>           deriving Show

> data NodeId = UnportedNodeId String
>             | PortedNodeId String Port
>             deriving Show

> data Stmt = NullStmt
>           | Stmt :# Stmt
>           | NodeStmt NodeId AttrList
>           | EdgeStmt EdgeOperand EdgeRhs AttrList
>           | SubgraphStmt String Stmt
>           deriving Show

> data Graphviz = Graph String Stmt
>               | Digraph String Stmt
>               deriving Show

> infixr 5 :#
> infixr 5 :@
> infixr 5 :&

> compile :: Graphviz -> String
> compile (Graph name b) = "graph " ++ name ++ " {\n" ++ compileStmt b ++ ";\n}"
> compile (Digraph name b) = "digraph " ++ name ++ " {\n" ++ compileStmt b ++ ";\n}"

> compileNodeId :: NodeId -> String
> compileNodeId (UnportedNodeId a) = a
> compileNodeId (PortedNodeId a b) = a ++ ":" ++ compilePort b

> compilePort :: Port -> String
> compilePort (CompassPort dir) = compileDirection dir

> compileDirection :: CompassDirection -> String
> compileDirection North = "n"
> compileDirection Northeast = "ne"
> compileDirection East = "e"
> compileDirection Southeast = "se"
> compileDirection South = "s"
> compileDirection Southwest = "sw"
> compileDirection West = "w"
> compileDirection Northwest = "nw"
> compileDirection Centre = "c"
> compileDirection Underscore = "_"

> compileAttrList :: AttrList -> String
> compileAttrList NullAttrList = "[]"
> compileAttrList (a :& b) = compileAttrList a ++ compileAttrList b
> compileAttrList (AttrSet a) = "[" ++ compileAttr a ++ "]"

> compileAttr :: Attr -> String
> compileAttr (TruthAssignment a) = a
> compileAttr (a :=: b) = a ++ "=" ++ b
> compileAttr (a :@ b) = compileAttr a ++ ", " ++ compileAttr b

> compileEdgeOp :: EdgeOperand -> String
> compileEdgeOp (EdgeNodeId node) = node
> compileEdgeOp (Subgraph name contents)
>   = "subgraph " ++ name ++ " { " ++ compileStmt contents ++ " } "

> compileEdgeRhs :: EdgeRhs -> String
> compileEdgeRhs (CompoundEdgeRhs a b) = compileEdgeRhs a ++ compileEdgeRhs b
> compileEdgeRhs (DirectedEdge op) = "-> " ++ compileEdgeOp op

> compileStmt :: Stmt -> String
> compileStmt (a :# b) = compileStmt(a) ++ ";\n" ++ compileStmt b
> compileStmt (NodeStmt a b) = compileNodeId a ++ " " ++ compileAttrList b
> compileStmt (EdgeStmt lhs rhs attrs)
>   = compileEdgeOp lhs ++ " " ++ compileEdgeRhs rhs ++ " " ++ compileAttrList attrs
> compileStmt NullStmt = ""

> infixr 6 :=:

> sampleGraph = Digraph "Sample"
>                       (NodeStmt (PortedNodeId "funx" (CompassPort North))
>                                 (AttrSet(("label" :=: "bar")
>                                          :@ ("color" :=: "blue"))) :#
>                        NullStmt)