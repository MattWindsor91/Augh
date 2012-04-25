Augh.UmlLabel
=============

This module transforms UML class constructs to Graphviz HTML labels.
Believe it or not, this module is the workhorse of the UML to Graphviz
translator, because all the information pertaining to single classes
goes into the labels.

> module Augh.UmlLabel
>     ( makeClassLabel
>     , makeRelationEndLabel
>     )
> where

We'll need access to the UML algebraic data types.

> import Augh.UmlTypes
> import Control.Arrow ( ( *** ) )
> import Data.List
> import Language.Dot.Syntax
>     ( Id ( StringId )
>     , Xml ( XmlTag
>           , XmlEmptyTag
>           , XmlText
>           )
>     , XmlName ( XmlName )
>     , XmlAttribute ( XmlAttribute )
>     , XmlAttributeValue ( XmlAttributeValue )
>     )
> import Control.Applicative ( pure )

Assumed Knowledge
-----------------

This module makes significant use of left folds and pointfree style.

Much of the data types used in this module come from Augh.UmlTypes.


newLine stores the HTML code to insert a new line into the label as
well as the output code.

> newLine :: Xml
> newLine = XmlEmptyTag ( XmlName "br" ) []

Functions
---------

First of all, we need a few helper functions.

comp2 is a composition of compositions.

> comp2 :: ( a -> b ) -> ( c -> d -> a ) -> c -> d -> b
> comp2 = ( ( . ) . ( . ) )

htmlEncode takes a string as input and return as output that string
with any character sequences that could be mistaken for HTML escaped.
For example, < and > become &lt; and &gt;.

> htmlEncode :: String -> String
> htmlEncode = concatMap encodex
>   where
>     encodex '<'  = "&lt;"
>     encodex '>'  = "&gt;"
>     encodex '&'  = "&amp;"
>     encodex '\n' = "<BR />"
>     encodex a    = [a]

***

makeLeftRow takes XML foo makes a <tr><td>foo</td></tr>, with the
contents aligned left.

> makeLeftRow :: [ Xml ] -> Xml
> makeLeftRow = makeRow ( map toAttr [ ( "align",  "text" )
>                                    , ( "balign", "left" )
>                                    ]
>                       )

> makeRow :: [ XmlAttribute ] -> [ Xml ] -> Xml
> makeRow attrs = ( XmlTag ( XmlName "tr" ) [] )
>           . pure . ( XmlTag ( XmlName "td" ) attrs )

> toAttr :: ( String, String ) -> XmlAttribute
> toAttr = uncurry XmlAttribute . ( XmlName *** XmlAttributeValue )

***

compileSeparatedList, for a given abstract type A, takes in a XML
separator, a function compiling items of type A to XML, a list of
items of type A, and returns a XML containing the compiled forms of
each item in the list, separated by the separator (with no separators
at the start or the end).

> compileSeparatedList :: [ Xml ] -> ( a -> [ Xml ] ) -> [ a ] -> [ Xml ]
> compileSeparatedList sep
>     = ( intercalate sep ) `comp2` map

***

Currently, due to graphviz oddities, we use a rather archaic method of
showing static (a dollar sign prefix) to show static, as oppposed to
the modern underline convention.  Fixing this would be a neat
extension of the compiler!

> makeStatic :: [ Xml ] -> [ Xml ]
> makeStatic = ( ( XmlText "$" ) : )


Visibilities and Multiplicities
-------------------------------

This function converts a visibility marker to its HTML.

> makeVisibilityHtml :: Visibility -> Xml
> makeVisibilityHtml = XmlText . makeVisibilityStr

> makeVisibilityStr :: Visibility -> String
> makeVisibilityStr Private   = "-"
> makeVisibilityStr Protected = "#"
> makeVisibilityStr Public    = "+"

***

This function converts a multiplicity marker to its HTML.

> makeMultiplicityHtml :: Multiplicity -> String
> makeMultiplicityHtml ( FixedNumber i ) = show i
> makeMultiplicityHtml ( i :..: j ) = show i ++ ".." ++ show j
> makeMultiplicityHtml ( OpenRange i ) = show i ++ "..*"

***

> makeVariableHtml :: Variable -> [ Xml ]
> makeVariableHtml ( fname :- ftype )
>     = pure . XmlText . concat $ [ htmlEncode fname
>                                 , " : "
>                                 , htmlEncode ftype
>                                 ]

Field and Method Lists
----------------------

The field and method lists are handled so similarly that they derive
from the same core function makeComponentListHtml working on an
abstract list of line break separated components, differing only in
the per-item function that is passed to it.

> makeFieldsHtml :: [ Field ] -> [ Xml ]
> makeFieldsHtml = makeComponentListHtml makeFieldHtml

> makeMethodsHtml :: [ Method ] -> [ Xml ]
> makeMethodsHtml = makeComponentListHtml makeMethodHtml

MakeComponentListHtml itself is derived from our generic separated
list compile function, with the separator set as line break.

> makeComponentListHtml :: ( a -> [ Xml ] ) -> [ a ] -> [ Xml ]
> makeComponentListHtml
>     = ( pure . makeLeftRow ) `comp2`
>       ( compileSeparatedList [ newLine ] )


We use a left fold to build up the output string, essentially
repeatedly running makeMethodHtml on the next unconverted method and
appending the result to the output.


Fields
------

The function makeFieldHtml converts a field definition into HTML as
per the UML syntax for fields.

If the field is static, we just send what the outcome would be if it
weren't static to makeStatic, which applies the UML static annotation.

> makeFieldHtml :: Field -> [ Xml ]
> makeFieldHtml ( StaticField vis contents )
>     = makeStatic . makeFieldHtml $ Field vis contents
> makeFieldHtml ( Field visibility contents )
>     = intersperse ( XmlText " " ) ( makeVisibilityHtml visibility
>                                     : makeVariableHtml contents
>                                   )


Parameter Lists
---------------

The function makeParamsHtml takes a list of variables representing
formal parameters, and compiles them into a string of HTML that should
be placed in between the brackets representing the parameter list.

> makeParamsHtml :: [ Variable ] -> [ Xml ]

If there are less than two params, keep the param list on one line.

> makeParamsHtml []         = []
> makeParamsHtml (param:[]) = makeVariableHtml param

Otherwise, put all the params on their own line, to make sure the
class box doesn't become ridiculously wide.  Make sure the list starts
on a new line from the opening bracket and that the closing bracket
appears on a separate line (by encapsulating the whole thing in
newlines) for tidiness; also indent the parameters a bit.

> makeParamsHtml params
>     = ( [ newLine , XmlText "  " ] ++ ) . ( ++ [ newLine ] )
>       . ( compileSeparatedList [ newLine, XmlText "  " ]
>           makeVariableHtml ) $ params

> makeMethodHtml :: Method -> [ Xml ]

Static is a special case

> makeMethodHtml ( StaticMethod vis name params rtype )
>     = makeStatic . makeMethodHtml $ Method vis name params rtype

> makeMethodHtml ( Method visibility name params rtype )
>     = ( [ makeVisibilityHtml visibility
>         , XmlText ( " " ++ htmlEncode name ++ "(" )
>         ]
>         ++ makeParamsHtml params ++
>                [ XmlText ( ") : " ++ htmlEncode rtype ) ]
>       )

***

This function takes a class definition, and makes a label for it.

Because all the differences in class definitions amount to changes in
the name, the actual heavy lifting of the function is farmed out into
a separate inner function taking the actual parts of a class that make
up the label, with any name transformations already done.

> makeClassLabel :: ClassDef -> Xml
> makeClassLabel def
>   = makeClassLabelInner name
>                         (fields cls)
>                         (methods cls)
>   where
>     name = labelNameOfClassDef def
>     cls = classOfDef def
>     methods (Class _ _ methList) = methList
>     fields (Class _ fieldList _) = fieldList

***

> makeClassLabelInner :: String -> [Field] -> [Method] -> Xml
> makeClassLabelInner name fields methods
>     = XmlTag ( XmlName "table" ) attrs contents
>       where
>       attrs = map toAttr
>               [ ( "align"      , "left" )
>               , ( "border"     , "0"    )
>               , ( "cellborder" , "1"    )
>               , ( "cellspacing", "0"    )
>               ]
>       contents = concat [ [ makeTop name ]
>                         , makeFieldsHtml fields
>                         , makeMethodsHtml methods
>                         ]

> makeTop :: String -> Xml
> makeTop = ( makeRow [] ) . pure . XmlText . htmlEncode

***

Getting the name of a class definition as it will appear on the class
label is a matter of trying to reduce it down to something that
constructs from a real class, adding the necessary transformations to
the name along the way.

> labelNameOfClassDef :: ClassDef -> String
> labelNameOfClassDef (Abstract (Class name _ _)) = "<I>" ++ htmlEncode name ++ "</I>"
> labelNameOfClassDef (Concrete (Class name _ _)) = htmlEncode name
> labelNameOfClassDef (stereotype :>> def)
>   = htmlEncode ("<<" ++ stereotype ++ ">>\n") ++ labelNameOfClassDef def

Relationship labels
-------------------

> makeRelationEndLabel :: RelationEnd -> Id
> makeRelationEndLabel (EmptyRelationEnd _) = StringId ""
> makeRelationEndLabel (FullRelationEnd _ visibility name multiplicity)
>   = StringId . concat $ [ makeVisibilityStr visibility
>                         , " "
>                         , htmlEncode name
>                         , "\\n"
>                         , makeMultiplicityHtml multiplicity
>                         ]