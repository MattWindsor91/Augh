Augh.Uml
========

Part of Augh.
Licenced under the terms of the MIT Licence (see COPYING)

> module Augh.Uml
>     ( transformUmlList
>     )
> where

> import Augh.UmlLabel
> import Augh.UmlTypes
> import Language.Dot.Syntax
>     ( Graph ( Graph )
>     , GraphStrictness ( StrictGraph )
>     , GraphDirectedness ( DirectedGraph )
>     , Id ( NameId
>          , StringId
>          , XmlId
>          )
>     , Statement ( NodeStatement
>                 , EdgeStatement
>                 )
>     , Entity ( ENodeId )
>     , Attribute ( AttributeSetValue )
>     , NodeId ( NodeId )
>     , EdgeType ( DirectedEdge
>                , NoEdge
>                )
>     , Xml
>     )
> import Control.Arrow
>     ( ( &&& )
>     , first
>     )

Constants
---------

This is a set of Graphviz actions that are added to the top of any UML
graph, used to "set up" the graph before the main body of the graph.

> umlGraphvizPreamble :: Statement
> umlGraphvizPreamble = NodeStatement ( toNode "node" )
>                       ( toASV [ ( "shape", StringId "none" ) ] )

Transforming a list of statements
---------------------------------

The "main function", so to speak, of the UML transformer is the
following one, which takes a name to give the output graph and the
list of UML statements to put in the output graph, and spits out the
output graph in an algebraic data type format that can be used with
the compile function in Augh.Graphviz.

> transformUmlList :: String -> [ Uml ] -> Graph
> transformUmlList name umls
>     = Graph StrictGraph DirectedGraph ( Just . NameId $ name ) stms
>     where stms = umlGraphvizPreamble : map transformUml umls

UML statement transformer
-------------------------

For each UML statement to go into the graph, this function is invoked.
It converts a UML statement into a Graphviz statement, which itself
may be a composite of many Graphviz statements.

> transformUml :: Uml -> Statement
> transformUml ( ClassUml cls ) = transformUmlClass cls
> transformUml ( RelationUml rel ) = transformUmlRelationship rel

Classes
-------

The transformUmlClass function converts a class definition into a
Graphviz statement.  Really, however, the work of transforming one
into the other occurs in further functions; this function just glues
them together.

> transformUmlClass :: ClassDef -> Statement
> transformUmlClass = makeUmlNode . ( classOfDef &&& makeClassLabel )

***

This function makes the Graphviz node for a class, given the class and
the result of compiling a label for the node (which is an involved and
separate process described in the UmlLabel module).

> makeUmlNode :: ( Class, Xml ) -> Statement
> makeUmlNode ( Class name _ _, label )
>   = NodeStatement
>     ( NodeId ( ( NameId . safeName ) name ) Nothing )
>     [ AttributeSetValue ( NameId "label" ) ( XmlId label ) ]

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

safeName just appends the results of encoding every character.

> safeName :: ClassName -> String
> safeName = concatMap encodex
>  where
>    encodex 'Z'  = "ZZ"
>    encodex ' '  = "Zs"
>    encodex '\n' = "Zn"
>    encodex '<'  = "Zg"
>    encodex '>'  = "Zl"
>    encodex a    = [ a ]

Relationships
-------------

Relationships are mapped onto Graphviz edges.

> transformUmlRelationship :: Relationship -> Statement

:<>: is the composition relationship.

> transformUmlRelationship ( part :<>: whole )
>   = makeRelationshipEdge part whole ( toASV [ arrow, headl, taill ] )
>         where
>         arrow = ( "arrowhead", StringId "diamond" )
>         headl = ( "headlabel", makeRelationEndLabel whole )
>         taill = ( "taillabel", makeRelationEndLabel part )

***

:--: is the association relationship.

> transformUmlRelationship ( from :--: to )
>   = makeRelationshipEdge from to ( toASV [ arrow, headl, taill ] )
>         where
>         arrow = ( "arrowhead", StringId "none" )
>         headl = ( "headlabel", makeRelationEndLabel to )
>         taill = ( "taillabel", makeRelationEndLabel from )

***

:->: is the generalisation relationship.  It's different in that it
takes in two class names instead of two relation ends --
generalisations can't be backed up by fields!

> transformUmlRelationship ( sub :->: super )
>   = makeRelationshipEdge ( EmptyRelationEnd sub )
>                          ( EmptyRelationEnd super )
>     ( toASV [ ( "arrowhead", StringId "empty" ) ] )

***

> toASV :: [ ( String, Id ) ] -> [ Attribute ]
> toASV = map ( uncurry AttributeSetValue . ( first NameId ) )


Generic unported node ID

> toNode :: String -> NodeId
> toNode = ( flip NodeId $ Nothing ) . NameId

This is the general edge maker for relationships.

> makeRelationshipEdge :: RelationEnd -> RelationEnd -> [ Attribute ] -> Statement
> makeRelationshipEdge from to attrs
>     = EdgeStatement
>       [ ENodeId NoEdge       ( toNode . nameOf $ from )
>       , ENodeId DirectedEdge ( toNode . nameOf $ to   )
>       ] attrs
>     where
>     nameOf ( FullRelationEnd name _ _ _ ) = safeName name
>     nameOf ( EmptyRelationEnd name ) = safeName name
