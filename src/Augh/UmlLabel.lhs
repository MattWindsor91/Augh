Augh.UmlLabel
=============

This module transforms UML class constructs to Graphviz HTML labels.
Believe it or not, this module is the workhorse of the UML to Graphviz
translator, because all the information pertaining to single classes
goes into the labels.

> module Augh.UmlLabel where

We'll need access to the UML algebraic data types.

> import Augh.UmlTypes

Assumed Knowledge
-----------------

This module makes significant use of left folds and pointfree style.

Much of the data types used in this module come from Augh.UmlTypes.

Constants
---------

These are mainly bits of commonly used HTML that are factored out to
make the code cleaner.

fieldPreamble and methodPreamble are the bits of HTML that are
inserted before a list of fields and methods, respectively.

> fieldPreamble :: String
> fieldPreamble = "<TR><TD ALIGN=\"TEXT\" BALIGN=\"LEFT\">\n"

> methodPreamble :: String
> methodPreamble = "<TR><TD ALIGN=\"TEXT\" BALIGN=\"LEFT\">\n"

***

newLine stores the HTML code to insert a new line into the label as
well as the output code.

> newLine :: String
> newLine = "<BR />\n"

Functions
---------

First of all, we need a few helper functions.

htmlEncode and its inner recursion take a string as input and return
as output that string with any character sequences that could be
mistaken for HTML escaped.  For example, < and > become &lt; and &gt;.

> htmlEncode :: String -> String
> htmlEncode = foldl htmlEncodeInner []

htmlEncodeInner simply runs through the string moving each character
from the start of the input to the end of the output.  It appends the
next character in the input verbatim if it's already OK for HTML, or
an escaped replacement if it isn't.

>   where
>     htmlEncodeInner encoded x = (encoded ++ encodex x)
>     encodex '<'  = "&lt;"
>     encodex '>'  = "&gt;"
>     encodex '&'  = "&amp;"
>     encodex '\n' = "<BR />"
>     encodex a    = [a]

***

compileSeparatedList, for a given abstract type A, takes in a string
separator, a function compiling items of type A to String, a list of
items of type A, and returns a string containing the compiled forms of
each item in the list, separated by the separator (with no separators
at the start or the end).

> compileSeparatedList :: String -> (a -> String) -> [a] -> String
> compileSeparatedList sep compile = foldl foldTerm []

The first term of a ensures that the output is not prefixed by sep;
the second term places a separator between each term in the fold.

>   where
>     foldTerm [] a = compile a
>     foldTerm output a = concat [output, sep, compile a]

***

Finally, makeStatic takes any string of HTML and applies the UML
static annotation to it.

Currently, due to graphviz oddities, we use a rather archaic method of
showing static (a dollar sign prefix) to show static, as oppposed to
the modern underline convention.  Fixing this would be a neat
extension of the compiler!

> makeStatic :: String -> String
> makeStatic = ("$"++)

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
> makeVariableHtml (fname :- ftype)
>   = concat [htmlEncode fname, " : ", htmlEncode ftype]

Field and Method Lists
----------------------

The field and method lists are handled so similarly that they derive
from the same core function makeComponentListHtml working on an
abstract list of line break separated components, differing only in
the per-item function that is passed to it.

> makeFieldsHtml :: [Field] -> String
> makeFieldsHtml = makeComponentListHtml makeFieldHtml

> makeMethodsHtml :: [Method] -> String
> makeMethodsHtml = makeComponentListHtml makeMethodHtml

MakeComponentListHtml itself is derived from our generic separated
list compile function, with the separator set as line break.

> makeComponentListHtml :: (a -> String) -> [a] -> String
> makeComponentListHtml = compileSeparatedList "<BR />\n"


We use a left fold to build up the output string, essentially
repeatedly running makeMethodHtml on the next unconverted method and
appending the result to the output.


Fields
------

The function makeFieldHtml converts a field definition into HTML as
per the UML syntax for fields.

If the field is static, we just send what the outcome would be if it
weren't static to makeStatic, which applies the UML static annotation.

> makeFieldHtml :: Field -> String
> makeFieldHtml (StaticField visibility contents)
>   = (makeStatic . makeFieldHtml) (Field visibility contents)
> makeFieldHtml (Field visibility contents)
>   = (makeVisibilityHtml visibility) ++ " " ++ makeVariableHtml contents

Parameter Lists
---------------

The function makeParamsHtml takes a list of variables representing
formal parameters, and compiles them into a string of HTML that should
be placed in between the brackets representing the parameter list.

> makeParamsHtml :: [Variable] -> String

If there <= one param, keep it all one one line.

> makeParamsHtml []         = []
> makeParamsHtml (param:[]) = makeVariableHtml param

Otherwise, put all the params on their own line, to make sure the
class box doesn't become ridiculously wide.  Make sure the list starts
on a new line from the opening bracket and that the closing bracket
appears on a separate line (by encapsulating the whole thing in
newlines) for tidiness; also indent the parameters a bit.

> makeParamsHtml params
>   = concat
>     [ newLine,
>       "  ",
>       (compileSeparatedList ",<BR />\n  " makeVariableHtml params),
>       newLine ]

> makeMethodHtml :: Method -> String

Static is a special case

> makeMethodHtml (StaticMethod visibility name params return) =
>   (makeStatic . makeMethodHtml) (Method visibility name params return)

> makeMethodHtml (Method visibility name params return)
>   = concat [ (makeVisibilityHtml visibility),
>              " ",
>              htmlEncode name,
>              "(", makeParamsHtml params, ") : ",
>              htmlEncode return ]

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
>   ++ methodPreamble ++ makeMethodsHtml methods ++ "</TD></TR>\n"
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