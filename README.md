# Join-Patterns

## Grammar

```ebnf
receive = "receive" , rules ;
rules   = ( "{" , { rule } , "}" ) | rule ;
rule    = "(" , varargs , ")" , [ filter ] , "=>" , term ;
filter  = "if" , "(" , expr , ")" ;
expr    = "scala.quoted.Expr[Boolean]" ;
term    = "scala.quoted.Quotes.reflectModule.Tree.Statement.Term" ;
varargs = "scala.quoted.Varargs" ;
```
