Augh.Uml
========

Part of Augh!, the Anorexic UML to Graphviz compiler for Haskell
Licenced under the terms of the MIT Licence (see COPYING)

> module Augh.Uml where

> import Augh.Graphviz
> import Augh.UmlLabel
> import Augh.UmlTypes

Constants
---------

This is a set of Graphviz actions that are added to the top of any UML
graph, used to "set up" the graph before the main body of the graph.

> umlGraphvizPreamble :: Stmt
> umlGraphvizPreamble = NodeStmt (UnportedNodeId "node")
>                                (AttrSet ("shape" :=: "none"))

Transforming a list of statements
---------------------------------

The "main function", so to speak, of the UML transformer is the
following one, which takes a name to give the output graph and the
list of UML statements to put in the output graph, and spits out the
output graph in an algebraic data type format that can be used with
the compile function in Augh.Graphviz.

> transformUmlList :: String -> [Uml] -> Graphviz
> transformUmlList name statements
>   = Digraph name (transformUmlListInner umlGraphvizPreamble statements)

***

This function is the main, tail-recursive body of the statement list
transformer.

> transformUmlListInner :: Stmt -> [Uml] -> Stmt

BASE CASE: When there are no more UML statements left to transform,
return the list of transformed Graphviz statements.

> transformUmlListInner output [] = output

INDUCTIVE CASE: Invoke transformUmlListInner with the composite of the
current statement output and the transformation of the next UML
statement that needs transforming (the head of the Uml list), and the
rest of the untransformed UML statements.

> transformUmlListInner output (x:xs)
>   = transformUmlListInner output' xs
>     where output' = (output :# transformUml x)

UML statement transformer
-------------------------

For each UML statement to go into the graph, this function is invoked.
It converts a UML statement into a Graphviz statement, which itself
may be a composite of many Graphviz statements.

> transformUml :: Uml -> Stmt
> transformUml (a :> b) = transformUml a :# transformUml b
> transformUml (ClassUml cls) = transformUmlClass cls
> transformUml (RelationUml rel) = transformUmlRelationship rel
> transformUml _ = NullStmt

Classes
-------

The transformUmlClass function converts a class definition into a
Graphviz statement.  Really, however, the work of transforming one
into the other occurs in further functions; this function just glues
them together.

> transformUmlClass :: ClassDef -> Stmt
> transformUmlClass def
>   = makeUmlNode cls label
>     where
>       label = makeClassLabel def
>       cls = classOfDef def

***

This function makes the Graphviz node for a class, given the class and
the result of compiling a label for the node (which is an involved and
separate process described in the UmlLabel module).

> makeUmlNode :: Class -> String -> Stmt
> makeUmlNode (Class name fields methods) label
>   = NodeStmt (UnportedNodeId (safeName name))
>              (AttrSet ("label" :=: label))

***

It would be nice to be able to use standard class names as node names
in Graphviz, but Graphviz won't be able to properly handle every type
of name we can throw at it.  For this reason, we have a function that
converts a class name into a string that is safe to be used as a class
name in Graphviz.

The invariant here is that every standard class name must map
deterministically and in a manner sufficiently approximating
one-to-one, so that a safe conversion of a class name can feasibly
only stand for one class name.

As usual, safeName is defined as the initial invocation of a
tail-recursive procedure...

> safeName :: ClassName -> String
> safeName name = safeNameInner [] name

... which we'll define here.

> safeNameInner :: String -> String -> String

BASE CASE: When the input string is empty, the result is the output
string.

> safeNameInner output [] = output

INDUCTIVE CASE: Take the next character of input, perform a
transformation to it that will render it "safe", and recursively call
safeNameInner with the result appended to output and the rest of the
input presented as input.

> safeNameInner output (x:xs) = safeNameInner (output ++ encodex x) xs

We encode possibly unsafe characters with a system in which 'Z' is the
escape character, 'Z' is mapped to 'ZZ' and unsafe characters are
mapped to 'Zn' such as n is not 'Z'.

>   where
>     encodex 'Z'  = "ZZ"
>     encodex ' '  = "Zs"
>     encodex '\n' = "Zn"
>     encodex '<'  = "Zg"
>     encodex '>'  = "Zl"
>     encodex a    = [a]

Relationships
-------------

> transformUmlRelationship :: Relationship -> Stmt

> transformUmlRelationship (part :<>: whole)
>   = (EdgeStmt (EdgeNodeId (nameOf part))
>               (DirectedEdge (EdgeNodeId (nameOf whole)))
>               (AttrSet
>                (("arrowhead" :=: "diamond") :@
>                 ("headlabel" :=: makeRelationEndLabel whole) :@
>                 ("taillabel" :=: makeRelationEndLabel part))))
>   where
>     nameOf (FullRelationEnd name _ _ _) = (safeName name)
>     nameOf (EmptyRelationEnd name) = (safeName name)

> transformUmlRelationship (from :--: to)
>   = (EdgeStmt (EdgeNodeId (nameOf from))
>               (DirectedEdge (EdgeNodeId (nameOf to)))
>               (AttrSet
>                (("arrowhead" :=: "none") :@
>                 ("headlabel" :=: makeRelationEndLabel to) :@
>                 ("taillabel" :=: makeRelationEndLabel from))))
>   where
>     nameOf (FullRelationEnd name _ _ _) = (safeName name)
>     nameOf (EmptyRelationEnd name) = (safeName name)

> transformUmlRelationship (sub :->: super)
>   = (EdgeStmt (EdgeNodeId super)
>               (DirectedEdge (EdgeNodeId sub))
>               (AttrSet(("arrowhead" :=: "empty"))))
>   where
>     nameOf (FullRelationEnd name _ _ _) = (safeName name)
>     nameOf (EmptyRelationEnd name) = (safeName name)