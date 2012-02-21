Augh.UmlTypes
=============

Part of Augh!, the Anorexic UML to Graphviz compiler for Haskell
Licenced under the terms of the MIT Licence (see COPYING)

This module contains the algebraic data types abstractly representing
the subset of UML class models that we are interested in translating
to Graphviz.

> module Augh.UmlTypes where

We import GvTypes for the graphviz pragma function.

> import Augh.GvTypes

Type Synonyms
-------------

Defined here are some type synonyms we use to make things make more
sense semantically.

> type ClassName = String
> type Parameter = Variable
> type Identifier = String
> type TypeName = String

UML Statement
-------------

A UML (class diagram) statement is a class definition or a
relationship definition.  In addition, we recognise a "Graphviz
pragma", or a set of Graphviz statements.

> data Uml = Null
>          | ClassUml ClassDef
>          | RelationUml Relationship
>          | GvPragma Stmt
>          deriving (Show, Read)

Classes
-------

Now let's define classes, from the bottom up.

***

UML uses the "Name : Type" convention in both fields and method
parameters, so we'll represent it as an algebraic data type.  This
type is referred to as Variable, for lack of a better name, and we
use the :- operator as a surrogate for :.

> data Variable = Identifier :- TypeName
>                 deriving (Show, Read)

***

The visibility of a class element (method or field) determines which
external classes can use it.  It can be:

* Private (only the class in which it is defined can use it)
* Protected (only generalisations of the class and the class itself
  are allowed to use it)
* Public (any class can use it)

> data Visibility = Private
>                 | Protected
>                 | Public
>                 deriving (Show, Read)

***

A method is a function inside a class.  It has visibility, a name,
a set of parameters (which are modelled as Variables), and a return
type.

Static methods are methods that do not require an object instance of
the class - they belong to the class itself, and not any objects
derived from it.

> data Method = Method Visibility String [Parameter] TypeName
>             | StaticMethod Visibility String [Parameter] TypeName
>               deriving (Show, Read)

***

A field is a variable inside a class.  It has visibility, a name,
and a type (modelled as a Variable).

Static fields are fields that do not require an object instance,
just like static methods.

> data Field = Field Visibility Variable
>            | StaticField Visibility Variable
>              deriving (Show, Read)

***

And now, the class itself.  It's simply a combination of a name, a
series of fields, and a series of methods.

> data Class = Class String [Field] [Method]
>            deriving (Show, Read)

***

A class definition, simply enough, is a definition of a class together
with some implicit information about that class.

However, there are several different types of class definition:

1) A regular, concrete class;
2) An abstract class, modelled as an Abstract constructor taking a
   regular class;
3) A class definition with a stereotype attached to it, represented by
   a string with a :>> operator (mimicking the stereotype << and >>).

> data ClassDef = Concrete Class
>               | Abstract Class
>               | String :>> ClassDef
>               deriving (Show, Read)

For convenience, we'll define a function that gets the class part of
the ClassDef, throwing away any annotations.

> classOfDef :: ClassDef -> Class
> classOfDef (Concrete cls) = cls
> classOfDef (Abstract cls) = cls
> classOfDef (_ :>> def) = classOfDef def

Relationships
-------------

Relationships map two classes, which we refer to by name (string),
which we refer to as the "ends" of the relationship.

A relationship end can either represent a situation where the class
at that end is directly instantiated in the other end as an attribute
(we'll call this a "full" end) or where there is no such instantiation
and thus the relationship doesn't hold that way (an "empty" end).
Full ends come with an instance and multiplicity.

> data RelationEnd = FullRelationEnd ClassName Visibility Identifier Multiplicity
>                  | EmptyRelationEnd ClassName
>                  deriving (Show, Read)

***

A multiplicity determines how many instances of a given relationship
are allowed.

It can either be a fixed range [x .. y], an open range [x .. *] where
there is no upper limit, or a fixed number [x].

> data Multiplicity = FixedNumber Integer
>                   | Integer :..: Integer
>                   | OpenRange Integer
>                   deriving (Show, Read)

***

A relationship, in our UML subset, is either a composition (:<>:), an
association (:--:) or a generalisation (:->: or subclassing). (We
don't cover aggregations, but implementation thereof could be a useful
extension of this module.)  Generalisations can't be backed up by
attributes, so there is no such thing as a full relation end there,
and thus we just refer to the endpoint classes directly as strings.

In a generalisation, the first argument is the subclass; in a
composition, the first argument is the "whole" end.

> data Relationship = RelationEnd :<>: RelationEnd
>                   | RelationEnd :--: RelationEnd
>                   | ClassName :->: ClassName
>                   deriving (Show, Read)

Operators
---------

Here we define the precedence and associativity of our custom
operators.

> infixr 9 :-
> infixr 5 :>>
> infixr 5 :<>:
> infixr 5 :--:
> infixr 5 :..:
> infixr 5 :->: