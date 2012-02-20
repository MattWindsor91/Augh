This module transforms UML class constructs to Graphviz HTML labels.
Believe it or not, this module is the workhorse of the UML to Graphviz
translator, because all the information pertaining to single classes
goes into the labels.

> module Augh.UmlLabel where

We'll need access to the UML algebraic data types.

> import Augh.UmlTypes


Constants
---------

These are mainly bits of commonly used HTML that are factored out to
make the code cleaner.

fieldPreamble and methodPreamble are the bits of HTML that are
inserted before a list of fields and methods, respectively.

> fieldPreamble = "<TR><TD ALIGN=\"TEXT\" BALIGN=\"LEFT\">\n"
> methodPreamble = "<TR><TD ALIGN=\"TEXT\" BALIGN=\"LEFT\">\n"

Functions
---------

First of all, we need a few helper functions.

htmlEncode and its inner recursion take a string as input and return
as output that string with any character sequences that could be
mistaken for HTML escaped.  For example, < and > become &lt; and &gt;.

> htmlEncode :: String -> String
> htmlEncode str = htmlEncodeInner [] str

htmlEncodeInner simply runs through the string moving each character
from the start of the input to the end of the output.  It appends the
next character in the input verbatim if it's already OK for HTML, or
an escaped replacement if it isn't.

> htmlEncodeInner :: String -> String -> String
> htmlEncodeInner encoded [] = encoded
> htmlEncodeInner encoded (x:xs) = htmlEncodeInner (encoded ++ encodex x) xs
>     where
>       encodex '<'  = "&lt;"
>       encodex '>'  = "&gt;"
>       encodex '&'  = "&amp;"
>       encodex '\n' = "<BR />"
>       encodex a    = [a]

Visibilities and Multiplicities
-------------------------------

This function converts a visibility marker to its HTML.

> makeVisibilityHtml :: Visibility -> String
> makeVisibilityHtml Private = "-"
> makeVisibilityHtml Protected = "#"
> makeVisibilityHtml Public = "+"

***

This function converts a multiplicity marker to its HTML.

> makeMultiplicityHtml :: Multiplicity -> String
> makeMultiplicityHtml (FixedNumber i) = (show i)
> makeMultiplicityHtml (i :..: j) = (show i) ++ ".." ++ (show j)
> makeMultiplicityHtml (OpenRange i) = (show i) ++ "..*"

***

> makeVariableHtml :: Variable -> String
> makeVariableHtml (fname :- ftype) = htmlEncode fname ++ " : " ++ htmlEncode ftype

> makeFieldHtml :: Field -> String

Static is a special case

> makeFieldHtml (StaticField visibility contents) = "$" ++ makeFieldHtml (Field visibility contents)
> makeFieldHtml (Field visibility contents) = (makeVisibilityHtml visibility) ++ " " ++ makeVariableHtml contents

> makeFieldsHtmlInner :: String -> [Field] -> String
> makeFieldsHtmlInner [] [] = "&nbsp;"
> makeFieldsHtmlInner html [] = html
> makeFieldsHtmlInner html (x:xs) = makeFieldsHtmlInner (html ++ makeFieldHtml x ++ "<BR />\n") xs

> makeFieldsHtml :: [Field] -> String
> makeFieldsHtml fields = makeFieldsHtmlInner [] fields


> makeParamsHtmlInner :: String -> [Variable] -> String
> makeParamsHtmlInner html [] = html
> makeParamsHtmlInner [] (x:xs) = makeParamsHtmlInner (makeVariableHtml x) xs
> makeParamsHtmlInner html (x:xs) = makeParamsHtmlInner (html ++ ", " ++ makeVariableHtml x) xs

> makeParamsHtml :: [Variable] -> String
> makeParamsHtml params = makeParamsHtmlInner [] params

> makeMethodHtml :: Method -> String

Static is a special case

> makeMethodHtml (StaticMethod visibility name params return) =
>   "$" ++ makeMethodHtml (Method visibility name params return)

> makeMethodHtml (Method visibility name params return) =
>   (makeVisibilityHtml visibility)
>   ++ " " ++ htmlEncode name
>   ++ "(" ++ makeParamsHtml params ++ ") : "
>   ++ htmlEncode return

> makeMethodsHtmlInner :: String -> [Method] -> String
> makeMethodsHtmlInner [] [] = "&nbsp;"
> makeMethodsHtmlInner html [] = html
> makeMethodsHtmlInner html (x:xs) = makeMethodsHtmlInner (html ++ makeMethodHtml x ++ "<BR />\n") xs

> makeMethodsHtml :: [Method] -> String
> makeMethodsHtml fields = makeMethodsHtmlInner [] fields

***

This function takes a class definition, and makes a label for it.

Because all the differences in class definitions amount to changes in
the name, the actual heavy lifting of the function is farmed out into
a separate inner function taking the actual parts of a class that make
up the label, with any name transformations already done.

> makeClassLabel :: ClassDef -> String
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

> makeClassLabelInner :: String -> [Field] -> [Method] -> String
> makeClassLabelInner name fields methods =
>   "<<TABLE ALIGN=\"LEFT\" BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">\n"
>   ++ "<TR><TD>\n" ++ name ++ "</TD></TR>\n"
>   ++ fieldPreamble ++ makeFieldsHtml fields ++ "</TD></TR>\n"
>   ++ "<TR><TD ALIGN=\"LEFT\">\n" ++ makeMethodsHtml methods ++ "</TD></TR>\n"
>   ++ "</TABLE>>"

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

> makeRelationEndLabel :: RelationEnd -> String
> makeRelationEndLabel (EmptyRelationEnd _) = "\"\""
> makeRelationEndLabel (FullRelationEnd _ visibility name multiplicity)
>   = "\""
>     ++ makeVisibilityHtml visibility
>     ++ " "
>     ++ htmlEncode name
>     ++ "\\n"
>     ++ makeMultiplicityHtml multiplicity
>     ++ "\""