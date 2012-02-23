Augh.Graphviz
=============

Part of Augh!, the Anorexic UML to Graphviz compiler for Haskell
Licenced under the terms of the MIT Licence (see COPYING)

This module contains functions converting from the ADTs in the GvTypes
module into strings of concrete Dot (Graphviz) code.

> module Augh.Graphviz where
> import Augh.GvTypes
> import Data.List

> compile :: Graphviz -> String
> compile (Graph name b) = concat ["graph ", name, " {\n", compileStmt b, ";\n}"]
> compile (Digraph name b) = concat ["digraph ", name, " {\n", compileStmt b, ";\n}"]

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
> compileAttrList (a :& b) = compileAttrList a ++ compileAttrList b

To compile a list of attributes, return "[x0, x1, .., xn]" where xi is
the compiled form of each attribute in the list.  In order to get the
commas to separate the terms, we have a special case in our left fold
for the case where no output has been folded yet.

> compileAttrList (Attrs lst) = "[" ++ compiledLst lst ++ "]"
>   where
>     compiledLst = foldl listFold []
>     listFold [] y = compileAttr y
>     listFold x y = concat [x, ", ", compileAttr y]

> compileAttr :: Attr -> String
> compileAttr (a :=: "true") = a
> compileAttr (a :=: b) = a ++ "=" ++ b

> compileEdgeOp :: EdgeOperand -> String
> compileEdgeOp (EdgeNodeId node) = node
> compileEdgeOp (Subgraph name contents)
>   = concat ["subgraph ", name, " { ", compileStmt contents, " } "]

> compileEdgeRhs :: EdgeRhs -> String
> compileEdgeRhs (CompoundEdgeRhs a b) = compileEdgeRhs a ++ compileEdgeRhs b
> compileEdgeRhs (DirectedEdge op) = "-> " ++ compileEdgeOp op

Statement compiler
------------------

> compileStmt :: Stmt -> String
> compileStmt (a :# b) = concat [compileStmt a, ";\n", compileStmt b]
> compileStmt (NodeStmt a b) = concat [compileNodeId a, " ", compileAttrList b]
> compileStmt (EdgeStmt lhs rhs attrs)
>   =  intercalate " " [compileEdgeOp lhs, compileEdgeRhs rhs, compileAttrList attrs]
