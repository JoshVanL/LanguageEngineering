Bristol Tiny BASIC
==================

This document outlines a parser for a dialect of Tiny BASIC, a
language that came into existence in 1975. Edsger Dijkstra had this to
say about BASIC:

    "It is practically impossible to teach good programming to students
    that have had a prior exposure to BASIC: as potential programmers
    they are mentally mutilated beyond hope of regeneration."

On that jolly note, this document will proceed by describing a parser
for Tiny BASIC. Thankfully, the goal here is to learn to write a
parser, and not to learn BASIC itself. The hope that limited doses of
exposure will not affect minds too negatively.

The initial notes on Tiny BASIC came out of the "People's Computer
Company", Volume 4, Issue 2 in 1975. The PCC was a small-scale
publication in a fun and quirky format.

In PCC Issue Volume 4, Issue 5, p17, a note titled "Tiny Basic To
Go!" revealed that Tiny BASIC articles had become so popular that
they were to be moved to a new journal: "Dr Dobb's Journal of Tiny
Basic Calisthenics & Orthodontia".

The grammar in the present document is a modified version of John
Rible's version of Tiny BASIC. His version was published in PCC Volume
4, Issue 2, and essentially separates commands from statements. A
"small glitch" was noticed in the specification by Donald Hartley,
which was published in Vol 2 of Dr Dobbs.

Bristol Tiny BASIC continues to distinguish statements and commands,
but unlike Rible's Tiny BASIC, it does not separate these out into
different line non-terminals. Furthermore, Bristol Tiny BASIC
refactors carriage returns to the production rule for lines. These are
mostly cosmetic changes.

Grammar
-------

The grammar of a dialect of Tiny BASIC that shall henceforth be called
Bristol Tiny BASIC:

    prog : line*
    line : cmnd cr | stmt cr | number stmt cr
    cmnd : CLEAR | LIST | RUN
    stmt : PRINT args
         | IF expr rel expr THEN stmt
         | GOTO expr
         | INPUT vars
         | LET var '=' expr
         | GOSUB expr
         | RETURN
         | END
    args : ( string | expr ) (',' ( string | expr )* )
    vars : var (',' var)*
    expr : ['+'|'-'] term (('+'|'-') term)*
    term : fact (('*'|'/') fact)*
    fact : var | number | '(' expr ')'
    var : 'A' | 'B'  | ... | 'Z'
    number : digit digit*
    digit : '0' | '1' | ... | '9'
    rel : '<' [ '>' | '=' ] | '>' [ '<' | '=' ] | '='
    string : '"' achar* '"'
    achar: any character except '"' or '\n' or '\r'



Combinators
-----------

The grammar above will be used to produce a parser using Megaparsec,
which is a maintained version of Parsec, a parser combinator library
much like the one explored in lectures.

The combinators that are used here are given below. It is
worth memorizing at least the types of these operations.

< pure :: Applicative f => a -> f a

< (<$>) :: Functor f => (a -> b) -> f a -> f b
< (<$)  :: Functor f => a -> f b -> f a

< (<*>) :: Applicative f => f (a -> b) -> f a -> f b
< (<*)  :: Applicative f => f a -> f b -> f a
< (*>)  :: Applicative f => f a -> f b -> f b

< (<|>) :: Alternative f => f a -> f a -> f a
< many  :: Alternate f => f a -> f [a]
< some  :: Alternate f => f a -> f [a]

< sepBy  :: Alternative f => f a -> f sep -> f [a]
< sepBy1 :: Alternative f => f a -> f sep -> f [a]

< oneOf  :: [a] -> Parser Char
< noneOf :: [a] -> Parser Char

Notice that most of these do not mention `Parser` directly; a `Parser`
is in fact an instance of `Functor`, `Applicative`, `Alternative`, and
`Monad`: these concepts generalise to all kinds of other applications.

Parser
------

A few preliminaries that import modules and language features before
the full parser is defined.

> {-# LANGUAGE StandaloneDeriving #-}
> module TinyBasic where
> import Text.Megaparsec
> import Text.Megaparsec.String
> import Data.List (intercalate)

> cr :: Parser [Char]
> cr = many (oneOf "\r\n")

> tok :: String -> Parser String
> tok t = string t <* whitespace

> whitespace :: Parser ()
> whitespace = many (oneOf " \t") *> pure ()

Now each of the production rules in the grammar will be considered and
translated into a corresponding datatype and parser.

    program : line*

> data Prog = Prog [Line]

> prog :: Parser Prog
> prog = Prog <$> many line

    line : cmnd cr | stmt cr | number stmt cr

> data Line = Cmnd Cmnd | Stmt Stmt | Line Int Stmt
>
> line :: Parser Line
> line = (Cmnd <$> cmnd <* cr)
>    <|> (Stmt <$> stmt <* cr)
>    <|> (Line <$> number <*> stmt <* cr)



    cmnd : CLEAR | LIST | RUN

> data Cmnd = CLEAR | LIST | RUN

> cmnd :: Parser Cmnd
> cmnd = CLEAR <$ tok "CLEAR"
>    <|> LIST  <$ tok "LIST"
>    <|> RUN   <$ tok "RUN"


    stmt : PRINT args
         | IF expr rel expr THEN stmt
         | GOTO expr
         | INPUT vars
         | LET var '=' expr
         | GOSUB expr
         | RETURN
         | END

> data Stmt = PRINT [Either String Expr]
>           | IF Expr Rel Expr Stmt
>           | GOTO Expr
>           | INPUT [Ident]
>           | LET Ident Expr
>           | GOSUB Expr
>           | RETURN
>           | END




> stmt :: Parser Stmt
> stmt = PRINT   <$ tok "PRINT" <*> args
>    <|> IF      <$ tok "IF" <*> expr <*> rel <*> expr <* tok "THEN" <*> stmt
>    <|> GOTO    <$ tok "GOTO" <*> expr
>    <|> INPUT   <$ tok "INPUT" <*> vars
>    <|> LET     <$ tok "LET" <*> var <* tok "=" <*> expr
>    <|> GOSUB   <$ tok "GOSUB" <*> expr
>    <|> RETURN  <$ tok "RETURN"
>    <|> END     <$ tok "END"


    args : ( string | expr ) (',' ( string | expr )* )

> type Args = [Either String Expr]
> args :: Parser Args
> args = sepBy1 ((Left <$> str) <|> (Right <$> expr)) (tok ",")

    vars : var (',' var)*

> str :: Parser String
> str = tok "\"" *> some (noneOf ("\n\r\"")) <* tok "\""

    expr : ['+'|'-'] term (('+'|'-') term)*

> data Parity = POS | NEG
> data ExprOp = (:+:) Term | (:-:) Term
> data Expr   = Expr Parity Term [ExprOp]

> expr :: Parser Expr
> expr = Expr <$> ((POS <$ tok "+") <|> (NEG <$ tok "-") <|> pure POS)
>    <*> term <*> many exprOp

> exprOp :: Parser ExprOp
> exprOp = (:+:) <$ tok "+" <*> term
>      <|> (:-:) <$ tok "|" <*> term

    term : fact (('*'|'/') fact)*

> data Term = Term Fact [TermOp]
> data TermOp = MUL Fact | DIV Fact

> term :: Parser Term
> term = Term <$> fact <*> many termOp

> termOp :: Parser TermOp
> termOp = MUL <$ tok "*" <*> fact
>      <|> DIV <$ tok "/" <*> fact

    fact : var | number | '(' expr ')'
    var : 'A' | 'B'  | ... | 'Z'

> type Ident = Char

> data Fact = Var Ident
>             | Number Int
>             | Parens Expr

> fact :: Parser Fact
> fact = Var <$> var
>      <|> Number <$> number
>      <|> Parens <$ tok "(" <*> expr <* tok ")"

> var :: Parser Char
> var = oneOf ['A' .. 'Z'] <* whitespace

> vars :: Parser [Ident]
> vars = sepBy1 var (tok ",")

    number : digit digit*
    digit : '0' | '1' | ... | '9'

> number :: Parser Int
> number = (some (oneOf ['0' .. '9']) >>= return . read) <* whitespace

    rel : '<' [ '>' | '=' ] | '>' [ '<' | '=' ] | '='

> data Rel = (:<:) | (:<=:) | (:<>:) | (:=:) | (:>:) | (:>=:)
> rel :: Parser Rel
> rel = (:<>:)  <$ tok "<>"
>   <|> (:<>:)  <$ tok "><"
>   <|> (:=:)   <$ tok "="
>   <|> (:<=:)  <$ tok "<="
>   <|> (:<:)   <$ tok "<"
>   <|> (:>=:)  <$ tok ">="
>   <|> (:>:)   <$ tok ">"

> example = "LET X=3"

> parseFile :: FilePath -> IO ()
> parseFile filePath = do
>   file <- readFile filePath
>   putStrLn $ case parse prog filePath file of
>     Left err   -> parseErrorPretty err
>     Right prog -> pretty prog


Pretty Printing
---------------

The instances below allow values to be inspected in the terminal.
The default instance that is derived shows all the constructor names.

> deriving instance Show Prog
> deriving instance Show Line
> deriving instance Show Cmnd
> deriving instance Show Stmt
> deriving instance Show Expr
> deriving instance Show Term
> deriving instance Show TermOp
> deriving instance Show Fact
> deriving instance Show Parity
> deriving instance Show ExprOp
> deriving instance Show Rel

The pretty-printed output gives a version that should be acceptable
Tiny BASIC.

> class Pretty a where
>   pretty :: a -> String

> instance Pretty Prog where
>   pretty (Prog ls) = (unlines . map pretty) ls

> instance Pretty Line where
>   pretty (Line n stmt) = show n ++ " " ++ pretty stmt
>   pretty (Cmnd cmnd) = pretty cmnd
>   pretty (Stmt stmt) = pretty stmt

> instance Pretty Stmt where
>   pretty (PRINT es)                = "PRINT " ++ (intercalate ", " . map (either show pretty)) es
>   pretty (IF expr1 rel expr2 stmt) = "IF " ++ pretty expr1 ++ " " ++ pretty rel ++ " " ++ pretty expr2 ++ " " ++ pretty stmt
>   pretty (GOTO expr)               = "GOTO " ++ pretty expr
>   pretty (INPUT idents )           = "INPUT " ++ (intercalate ", " (map wrap idents))
>   pretty (LET ident expr)          = "LET " ++ wrap ident ++ " = " ++ pretty expr
>   pretty (GOSUB expr)              = "GOSUB " ++ show expr
>   pretty (RETURN)                  = "RETURN"
>   pretty (END)                     = "END"

> instance Pretty Cmnd where
>   pretty (CLEAR) = "CLEAR"
>   pretty (LIST)  = "LIST"
>   pretty (RUN)   = "RUN"

> instance Pretty Expr where
>   pretty (Expr NEG term exprOps) = "-" ++ pretty term ++ (concat . map pretty) exprOps
>   pretty (Expr POS term exprOps) = pretty term ++ (concat . map pretty) exprOps

> instance Pretty ExprOp where
>   pretty ((:+:) term) = " + " ++ pretty term
>   pretty ((:-:) term) = " - " ++ pretty term

> instance Pretty Rel where
>   pretty (:<>:) = "<>"
>   pretty (:=:)  = "="
>   pretty (:<=:) = "<="
>   pretty (:<:)  = "<"
>   pretty (:>=:) = ">="
>   pretty (:>:)  = ">"

> instance Pretty Term where
>   pretty (Term fact termOps) = pretty fact ++ (concat . map pretty) termOps

> instance Pretty TermOp where
>   pretty (MUL fact) = " * " ++ pretty fact
>   pretty (DIV fact) = " / " ++ pretty fact

> instance Pretty Fact where
>   pretty (Var ident)   = wrap ident
>   pretty (Number n)    = show n
>   pretty (Parens expr) = "(" ++ pretty expr ++ ")"

> wrap :: Char -> String
> wrap c = [c]

References
----------

[People's Computer Company](https://searchworks.stanford.edu/view/qk149zg0233)
[Dr Dobb's Journal](http://6502.org/documents/publications/dr_dobbs_journal/)
