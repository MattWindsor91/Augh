Augh!
=====

Augh! is a program that takes in a Read-able algebraic data type
construction representing a UML class diagram, and spits out a
Graphviz representation that can be used to render that diagram.

Licence
-------

Augh! is free software, distributed under the terms of the Expat/MIT
licence.  Any requests for the software to be licenced under different
terms should be sent to the author.

Usage
-----

The main program accepts in a read-able form of a UML statement list
in standard input, and produces a Graphviz Dot-language graph in
standard output.  The program is intended to be used under Unix-like
shells as follows:

    $ augh <sample.augh | dot -Tpdf >sample.pdf

See sample.augh for an example of real-world Augh! usage, or the
Augh.UmlTypes module for a listing of the algebraic data type read by
Augh!.

Background
----------

Augh! was written by Matt Windsor during his Software Engineering
Project at the University of York, to streamline the marking-down of
UML class diagrams.

It is named after the sorts of noise that the very mention of UML
causes functional programming fans to make.

Known Issues
------------

- Due to experiences of issues with graphviz in this area, static
  attributes are not marked up with underlines as modern UML
  convention dictates, but are instead prefixed with a dollar sign (an
  older and less widespread convention).

- Graphviz is somewhat deficient when it comes to properly placing
  edge labels.  As such, multiplicity/relationship realisation markers
  may look bad.

- Augh! does not implement the full breadth of features in a UML class
  diagram, nor does it implement the whole of the Graphviz language.
  The feature set it has (at time of writing) was fleshed out to the
  stage where enough was implemented to create the required UML
  diagram for the aforementioned project.

- The input must be in a Haskell-based format; a more human-readable
  format should be created later.