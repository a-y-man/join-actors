# Join-Patterns

## Previous works

[Scala Joins Library](https://lampwww.epfl.ch/~phaller/joins/index.html) (scala 2.6)

[akka](https://github.com/akka/akka/)

**Cω** : C# extension adding chords (join-patterns)

## Project

### Title

Join-Patterns for the Actor Model in Scala 3 using Macros

Join-Patterns til Actor Model i Scala 3 med Makroer

### Description

This project aims to implement actor-based systems with Join Patterns to process messages in Scala 3 by extending the language using its new macro environment. Metaprogramming enabled by macros relies on compile time logic to transform and generate code, that will provide a convenient integration into a codebase by leveraging the pattern-matching mechanism. The resulting artifact will demonstrate a correct and efficient implementation in regard of the existing solutions making use of the Actor model.

### Start and end date

14/02/2022 - 15/07/2022

## Scala 3 Macros

> Quoting takes expressions of type `T` to expressions of type `Expr[T]` and it takes types `T` to expressions of type `Type[T]`.
> 
> Splicing takes expressions of type `Expr[T]` to expressions of type `T` and it takes expressions of type `Type[T]` to types `T`.

## Grammar

```bnf
ReceiveClause ::=  ‘receive’ <<< RuleClauses >>>
RuleClauses   ::=  RuleClause { RuleClause }
RuleClause    ::=  ‘case’ Pattern [Guard] ‘=>’ Term ; Expr.block ?
Pattern       ::=  ‘(’ Varargs ‘)’
                |  Varargs ; simple argument
Term          ::=  ‘quotes.reflect.Term’
Varargs       ::=  ‘scala.quoted.Varargs’
```

```bnf
ReceiveClause ::=  ‘match’ <<< RuleClauses >>>                  Receive(expr, cases)

RuleClauses   ::=  RuleClause { RuleClause }                    Receive(EmptyTree, cases)
RuleClause    ::=  ‘case’ Pattern [Guard] ‘=>’ Block            RuleDef(pat, guard?, block)   // block starts at =>

Pattern       ::=  Pattern1 { ‘|’ Pattern1 }                    Alternative(pats)
Pattern1      ::=  SimplePattern 
SimplePattern ::=  PatVar                                       Ident(wildcard)
                |  Literal                                      Bind(name, Ident(wildcard))
                |  ‘(’ [Patterns] ‘)’                           Parens(pats) Tuple(pats)
                |  Quoted
                |  [TypeArgs] [ArgumentPatterns]
```

use `MatchType` ?

### Model

[Scala 3 Syntax Summary](https://dotty.epfl.ch/docs/internals/syntax.html#expressions)

```bnf
MatchClause      ::=  ‘match’ <<< CaseClauses >>>                  Match(expr, cases)

CaseClauses      ::=  CaseClause { CaseClause }                    Match(EmptyTree, cases)
CaseClause       ::=  ‘case’ Pattern [Guard] ‘=>’ Block            CaseDef(pat, guard?, block)   // block starts at =>

Pattern          ::=  Pattern1 { ‘|’ Pattern1 }                    Alternative(pats)
Pattern1         ::=  Pattern2 [‘:’ RefinedType]                   Bind(name, Typed(Ident(wildcard), tpe))
Pattern2         ::=  [id ‘@’] InfixPattern                        Bind(name, pat)
InfixPattern     ::=  SimplePattern { id [nl] SimplePattern }      InfixOp(pat, op, pat)
SimplePattern    ::=  PatVar                                       Ident(wildcard)
                   |  Literal                                      Bind(name, Ident(wildcard))
                   |  ‘(’ [Patterns] ‘)’                           Parens(pats) Tuple(pats)
                   |  Quoted
                   |  SimplePattern1 [TypeArgs] [ArgumentPatterns]
                   |  ‘given’ RefinedType
SimplePattern1   ::=  SimpleRef
                   |  SimplePattern1 ‘.’ id
PatVar           ::=  varid
                   |  ‘_’
Patterns         ::=  Pattern {‘,’ Pattern}
ArgumentPatterns ::=  ‘(’ [Patterns] ‘)’                           Apply(fn, pats)
                   |  ‘(’ [Patterns ‘,’] PatVar ‘*’ ‘)’
```
