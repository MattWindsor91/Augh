| Parser for Augh's external representation of UML.

Part of Augh.
Licenced under the terms of the MIT Licence (see COPYING)

> module Augh.Parser
>     ( runParser -- :: String -> [ Uml ]
>     )
> where
> import Augh.UmlTypes
> import Text.ParserCombinators.Parsec
>     ( Parser
>     , ParseError
>     , parse
>     , many
>     , many1
>     , digit
>     , char
>     , string
>     , eof
>     , space
>     , newline
>     , alphaNum
>     , skipMany
>     , between
>     , sepBy
>     , sepEndBy
>     , option
>     , anyChar
>     , manyTill
>     )
> import Control.Applicative
>     ( ( <|> ) -- Choice
>     , ( *> )  -- Do both, discard value on left
>     , ( <* )  -- Do both, discard value on right
>     , ( <*> )
>     , ( <$> ) -- fmap
>     , pure
>     )
> import Control.Monad ( void )


Entry point
-----------

> runParser :: String -> Either ParseError [ Uml ]
> runParser = parse parser "(unknown)"

> parser :: Parser [ Uml ]
> parser = many parseUmlSeparated <* eof


Convenience
-----------

> gap :: Parser ()
> gap = skipMany
>       ( void parseComment <|> void newline <|> void space )

> parseComment :: Parser String
> parseComment = char '#' *> manyTill anyChar ( void newline <|> eof )

> gapCharGap :: Char -> Parser Char
> gapCharGap = ( between gap gap ) . char

> stringGap :: String -> Parser String
> stringGap = ( <* gap ) . string

> idString :: Parser String
> idString = many1 ( alphaNum <|> char '_' <|> char '-' )

> betweenCharGaps :: Char -> Char -> Parser a -> Parser a
> betweenCharGaps open close
>     = between ( char open *> gap ) ( gap *> char close )


Uml
---

> parseUmlSeparated :: Parser Uml
> parseUmlSeparated = between gap gap parseUml

> parseUml :: Parser Uml
> parseUml = parseClassUml <|> parseRelationUml

> parseClassUml :: Parser Uml
> parseClassUml
>     = ClassUml <$> ( stringGap "CLASS:" *> parseClassDef )

> parseRelationUml :: Parser Uml
> parseRelationUml
>     = RelationUml <$> ( stringGap "REL:" *> parseRelationship )


ClassDef
--------

> parseClassDef, parseAbstract, parseStereotype, parseConcrete
>     :: Parser ClassDef

> parseClassDef
>     = parseAbstract <|> parseStereotype <|> parseConcrete

> parseAbstract = Abstract <$> ( stringGap "abstract" *> parseClass )

> parseStereotype
>     = ( :>> )
>       <$> ( between ( string "<<" ) ( string ">>" ) idString )
>       <*> ( gap *> parseClassDef )

> parseConcrete = Concrete <$> parseClass


Class
-----

> parseClass :: Parser Class
> parseClass
>     = Class
>       <$> ( idString {- className -} <* gapCharGap '{' )
>       <*> ( sepEndBy parseField  gap <* gapCharGap ';' )
>       <*> ( sepEndBy parseMethod gap <* gapCharGap '}' )


Field
-----

> parseField :: Parser Field
> parseField
>     = ( parseStatic *> parseFieldGivenConstructor StaticField )
>       <|> parseFieldGivenConstructor Field

> parseFieldGivenConstructor :: ( Visibility -> Variable -> Field )
>                               -> Parser Field
> parseFieldGivenConstructor cn
>     = cn <$> ( parseVisibility <* gap ) <*> parseVariable


Visibility
----------

> parseVisibility, parsePublic, parseProtected, parsePrivate
>     :: Parser Visibility

> parseVisibility
>     = parsePublic <|> parseProtected <|> parsePrivate

> parsePublic    = char '+' *> return Public
> parseProtected = char '#' *> return Protected
> parsePrivate   = char '-' *> return Private


Variable
--------

> parseVariable :: Parser Variable
> parseVariable
>     = ( :- ) <$> ( idString <* gapCharGap ':' ) <*> idString


Method
------

> parseMethod :: Parser Method
> parseMethod
>     = ( parseStatic *> parseMethodGivenConstructor StaticMethod )
>       <|> parseMethodGivenConstructor Method

> parseMethodGivenConstructor ::
>     ( Visibility -> String -> [ Parameter ] -> TypeName -> Method )
>     -> Parser Method
> parseMethodGivenConstructor cons
>     = cons
>       <$> parseVisibility <* gap
>       <*> idString <* gap -- name
>       <*> betweenCharGaps '(' ')'
>               ( sepBy parseVariable ( gapCharGap ',' ) )
>               <* gapCharGap ':'
>       <*> idString -- typename


Relationship
------------

This one's a bit complicated, due to possibly having a full
relationship end, an empty relationship end or just a class name (the
latter two being indistinguishable at first glance)

> parseRelationship :: Parser Relationship
> parseRelationship
>     = ( idString <* gap ) >>=
>          ( <|> ) <$> parseFullLhsRel <*> parseEmptyLhsRel


> parseFullLhsRel :: ClassName -> Parser Relationship
> parseFullLhsRel className
>     = ( parseFull className <* gap ) >>=
>          ( <|> ) <$> parseCompose <*> parseAssociate

> parseFull :: ClassName -> Parser RelationEnd
> parseFull className
>     = betweenCharGaps '(' ')'
>       ( ( FullRelationEnd className )
>         <$> ( parseVisibility <* gap )
>         <*> ( idString <* gap ) -- identifier
>         <*> ( betweenCharGaps '[' ']' parseMultiplicity )
>       )

> parseEmptyLhsRel :: ClassName -> Parser Relationship
> parseEmptyLhsRel className
>     = parseCompose end <|>
>       ( char '-' *> ( parseAssociate end
>                       <|> parseGeneralise className
>                     )
>       ) where end = EmptyRelationEnd className

> -- | Given the LHS, parses a composing relationship.
> parseCompose    :: RelationEnd -> Parser Relationship
> parseCompose    = ( <$> ( stringGap "<>" *> parseRhs ) ) . ( :<>: )

> -- | Given the LHS, parses an associating relationship.
> parseAssociate  :: RelationEnd -> Parser Relationship
> parseAssociate  = ( <$> ( stringGap "-"  *> parseRhs ) ) . ( :--: )

> -- | Given the LHS, parses a generalising relationship.
> parseGeneralise :: ClassName -> Parser Relationship
> parseGeneralise = ( <$> ( stringGap ">" *> idString  ) ) . ( :->: )

> -- | Parses the right hand side of a non-generalising relationship.
> parseRhs :: Parser RelationEnd
> parseRhs = ( idString <* gap ) >>= parseRhsAfterClass

> parseRhsAfterClass :: ClassName -> Parser RelationEnd
> parseRhsAfterClass = option <$> EmptyRelationEnd <*> parseFull


Multiplicity
------------

> parseMultiplicity :: Parser Multiplicity
> parseMultiplicity
>     = read <$> ( many1 digit <* gap )
>       >>= option <$> FixedNumber <*> parseRangeMultiplicity

> parseRangeMultiplicity, parseOpenRangeRhs, parseClosedRangeRhs
>     :: Integer -> Parser Multiplicity

> parseRangeMultiplicity
>     = ( stringGap ".." *> ) .
>       ( ( <|> ) <$> parseOpenRangeRhs <*> parseClosedRangeRhs )

> parseOpenRangeRhs = ( char '*' *> ) . pure . OpenRange
> parseClosedRangeRhs = ( <$> ( read <$> many1 digit ) ) . ( :..: )

Static
------

> parseStatic :: Parser ()
> parseStatic = char '$' *> gap
