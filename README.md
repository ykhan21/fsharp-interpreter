# fsharp-interpreter
An interpreter made in F# for a custom language featuring tagged unions, pattern matching, and function lambdas.

The interpreter takes expressions (programs) made in the custom language and evaluates them.

## Expressions
The expressions range as follows: integer and string constants, binary operators (plus, minus, times, and divide), let and variable expressions, equality and conditional expressions, function call and declaration expressions, and first-class function call and declaration expressions.
They are represented with an F# tagged union:
```
type Expression =
    | IntConstant of int32
    | StrConstant of string
    | BinOp of Operator * Expression * Expression
    | Let of Name * Expression * Expression
    | Variable of Name
    | Equality of Expression * Expression
    | If of Expression * Expression * Expression
    | FunDeclaration of Name * Name list * Expression * Expression
    | FunCall of Name * Expression list
    | FirstClassFunDeclaration of Name * Name list * Expression
    | FirstClassFunCall of Expression * Expression list
    
type Operator = PLUS | MINUS | TIMES | DIV
type Name = string
```

##Variables

## Sample programs
### p1
Just a constant that evaluates to 343
```
let p1 = IntConstant(343)
```
