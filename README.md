# fsharp-interpreter
An interpreter made in F# for a custom language featuring tagged unions, pattern matching, and function lambdas.

The interpreter takes Expressions (programs) made in the custom language and evaluates them, returning Values.

## Expressions
The expressions range as follows: integer and string constants, binary operators (plus, minus, times, and divide), let and variable expressions, equality and conditional expressions, function call and declaration expressions, and first-class function call and declaration expressions.

They are represented with an F# tagged union:
```fsharp
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

## Values
Values are what the interpreter returns upon evaluating and expression.

They are as follows:
```fsharp
type Value =
    | IntValue of int32
    | StrValue of string
    | BoolValue of bool
    | FunValue of (Value list -> Value)
```

## Sample programs
### p1 (integer constants)
Just a constant that evaluates to 343 (i.e., IntValue(343))
```fsharp
let p1 = IntConstant(343)
```
### p2 (formatting pending)
```fsharp
// 555 / 5
let p2 = BinOp(DIV,
               IntConstant(555),
               IntConstant(5))

// (let var1 = (555 / 5) in var1 * -1)
let p3 = Let("var1",
             p2,
             BinOp(TIMES,
                   Variable("var1"),
                   IntConstant(-1)))

// (555 / 5) == 474
let p4 = Equality(p2,IntConstant(474))

// (let dividend = 474 in
//   (let divisor = 474 in
//     (divisor == 0) ? 0 : dividend / divisor))
let p5 = Let("dividend",
              IntConstant(474),
              Let("divisor",
                  IntConstant(0),
                  If(Equality(Variable("divisor"),IntConstant(0)),
                     IntConstant(0),
                     BinOp(DIV,Variable("dividend"),Variable("divisor")))))


let p6 = FunDeclaration("f",["top";"bot"],
                        If(Equality(Variable("bot"),IntConstant(0)),
                           IntConstant(0),
                           BinOp(DIV,Variable("top"),Variable("bot"))),
                        Let("bot",IntConstant(3),
                            BinOp(PLUS,
                                  Let("bot",IntConstant(2),Variable("bot")),
                                  BinOp(PLUS,
                                        FunCall("f",[IntConstant(474);Variable("bot")]),
                                        FunCall("f",[IntConstant(474);IntConstant(0)])))))

//// let ff = fun sum(x) -> x+1 in
////   ff(4)
let p16 = Let("ff",
              FirstClassFunDeclaration("sum",["x"],BinOp(PLUS,Variable("x"),IntConstant(1))),
              FirstClassFunCall(Variable("ff"),[IntConstant(4)]))
```

