
open System

type Operator = PLUS | MINUS | TIMES | DIV
type Name = string

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

type Value =
    | IntValue of int32
    | StrValue of string
    | BoolValue of bool
    | FunValue of (Value list -> Value)

type Environment = (Name*Value) list
type FunEnvironment = (Name * (Value list -> Value)) list

// returns the value associated with the name in the env
let rec lookup (name:Name) (env:Environment) =
    match env with
    | [] -> raise (System.Exception("NoSuchVariable: "+name))
    | (nm,value)::tl when nm=name -> value
    | _::tl -> lookup name tl

// returns the function labmda associated with the name in the env
let rec funLookup (name:Name) (knownFunctions:FunEnvironment) =
    match knownFunctions with
    | [] -> raise (System.Exception("NoSuchFunction: "+name))
    | (nm,myFunc)::_ when nm=name -> myFunc
    | _::tl -> funLookup name tl

// the interpreter
let rec eval c (env:Environment) (knownFunctions:FunEnvironment) =
    let rec createFunEnv (formalArgs:Name list) (actualValues:Value list) envSoFar =
        match formalArgs, actualValues with
        | [], [] -> envSoFar
        | [], hd::_ -> raise (System.ArgumentException("createFunEnv: unequal lengths of formalArgs and actualArgs"))
        | hd::_, [] -> raise (System.ArgumentException("createFunEnv: unequal lengths of formalArgs and actualArgs"))
        | name::t1, value::t2 -> createFunEnv t1 t2 ((name, value)::envSoFar)

    match c with
    | IntConstant(value) -> (IntValue value)
    | StrConstant(value) -> (StrValue value)
    | BinOp(op,left,right) ->
        let (IntValue l) = eval left env knownFunctions
        let (IntValue r) = eval right env knownFunctions
        match op with
        | PLUS  -> IntValue (l+r)
        | MINUS -> IntValue (l-r)
        | TIMES -> IntValue (l*r)
        | DIV   -> IntValue (l/r)
    | Let(name,value,body) ->
        let v:Value = eval value env knownFunctions
        let newE = (name,v)::env
        eval body newE knownFunctions
    | Variable(name) -> lookup name env
    | Equality(left,right) ->
        let (IntValue l) = eval left env knownFunctions
        let (IntValue r) = eval right env knownFunctions
        BoolValue(l=r)
    | If(cond,ifSide,elseSide) ->
        let (BoolValue condition) = eval cond env knownFunctions
        if condition then eval ifSide env knownFunctions
                     else eval elseSide env knownFunctions
    | FunDeclaration(name,formalArgs,body,scope) ->
        let myFunc = fun (actualValues : Value list) ->
            let envThatKnowsTheArgs:Environment = createFunEnv formalArgs actualValues env
            eval body envThatKnowsTheArgs knownFunctions
        let newKnownFunctions = (name,myFunc) :: knownFunctions
        eval scope env newKnownFunctions
    | FunCall(name,actualArgs) ->
        let myFunc = funLookup name knownFunctions
        let actualValues = List.map (fun arg -> eval arg env knownFunctions) actualArgs
        myFunc actualValues
    | FirstClassFunDeclaration(name, formalArgs, body) ->
        let myFunc = fun (actualValues : Value list) ->
            let envThatKnowsTheArgs:Environment = createFunEnv formalArgs actualValues env
            eval body envThatKnowsTheArgs knownFunctions
        FunValue(myFunc)
    | FirstClassFunCall(funToBeCalled,actualArgs) ->
        let (FunValue myFunc) = eval funToBeCalled env knownFunctions
        let actualValues = List.map (fun arg -> eval arg env knownFunctions) actualArgs
        myFunc actualValues


// ***** sample programs start ***** //

// 343
let p1 = IntConstant(343)

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


 // ***** programs end ***** //


[<EntryPoint>]
let main argv =
    let env:Environment = []
    let knownFunctions:FunEnvironment = []

    printfn "Value: %A" (eval p16 env knownFunctions)

    0 // return an integer exit code




 // ***** extra sample programs end   ***** //

// 
//let p5 = FunDeclaration("f",["bot"],
//                        Variable("bot"),
//                        Let("z",IntConstant(3),
//                            FunCall("f",[Variable("z")])))

//// 343
//let p1 = IntConstant(343)

//// "343"
//let p2 = StrConstant("343")

//// 400 + 74
//let p3 = BinOp( PLUS, IntConstant(400), IntConstant(74) )

//// 555 / 5
//let p4 = BinOp( DIV, IntConstant(555), IntConstant(5) )

//// (555 / 5) / 3
//let p5 = BinOp( DIV, p4, IntConstant(3) )

//// (let var1 = (400 + 74) in var1 * -1)
//let p6 = Let("var1", p3, BinOp( TIMES, Variable("var1"), IntConstant(-1) ))

//// (let var2 = 
////           (let var1 = (400 + 74) in var1 * -1)
////  in var2 / 2
//// )
//let p7 = Let("var2", p6, BinOp( DIV, Variable("var2"), IntConstant(2) ))

//// (let v1 = 400 in
////     (let v2 = 74 in v1 + v2))
//let p11 = Let("v1", IntConstant(400), Let("v2", IntConstant(74), BinOp( PLUS, Variable("v1"), Variable("v2") ) ))

//// (let var = 400 in
////    (let var = 70 in var)
////    +
////    4 + var
//// )
//let p12 = Let("var",
//              IntConstant(400),
//              BinOp(PLUS,
//                    Let("var",IntConstant(70),Variable("var")), //enters its own environment, so it's not 144
//                    BinOp(PLUS,
//                          IntConstant(4),
//                          Variable("var")))
//              )

//// (let var = 3 in var == 0)
//let p8 = Let("var", IntConstant(3), Equality(Variable("var"),IntConstant(0)))

//// (400 + 74) == 474
//let p9 = Equality(p3,IntConstant(474))

//// (let dividend = 474 in
////   (let divisor = 474 in
////     (divisor == 0) ? 0 : dividend / divisor))
//let p10 = Let("dividend",
//              IntConstant(474),
//              Let("divisor",
//                  IntConstant(474),
//                  If(
//                     Equality(Variable("divisor"),IntConstant(0)),
//                     IntConstant(0),
//                     BinOp(DIV,Variable("dividend"),Variable("divisor"))
//                     )
//                 )
//              )

//// fun safeDivision(dividend, divisor) -> ((divisor == 0) ? 0 : dividend / divisor)
////   safeDivision(400 + 74, 0) + safeDivision(474, 1)
//let p13 = FunDeclaration("safeDivision",
//                         ["dividend";"divisor"],
//                         If(Equality(Variable("divisor"),IntConstant(0)),
//                            IntConstant(0),
//                            BinOp(DIV,Variable("dividend"),Variable("divisor"))
//                            ),
//                         BinOp(PLUS,
//                               FunCall("safeDivision",[p3;IntConstant(0)]),
//                               FunCall("safeDivision",[IntConstant(474);IntConstant(1)]))
//                         )
//// fun sum(x, y) -> (x+y)
////   sum(473 + 1)
//let p14 = FunDeclaration("sum",
//                         ["x";"y"],
//                         BinOp(PLUS,Variable("x"),Variable("y")),
//                         FunCall("sum",[IntConstant(473);IntConstant(1)]))

//// fun sum(x, y) -> (x+y)
////   fun safeDivision(dividend, divisor) -> ((divisor == 0) ? sum(1,1) : dividend / divisor)
////     safeDivision(400 + 74, 0) + safeDivision(474, 1)
//let p15 = FunDeclaration("sum",
//                         ["x";"y"],
//                         BinOp(PLUS,Variable("x"),Variable("y")),
//                         FunDeclaration("safeDivision",
//                                        ["dividend";"divisor"],
//                                        If(Equality(Variable("divisor"),IntConstant(0)),
//                                           FunCall("sum",[IntConstant(1);IntConstant(1)]),
//                                           BinOp(DIV,Variable("dividend"),Variable("divisor"))),
//                                        BinOp(PLUS,
//                                              FunCall("safeDivision",[p3;IntConstant(0)]),
//                                              FunCall("safeDivision",[IntConstant(474);IntConstant(1)]))))

//// let ff = fun add1(x) -> x+1 in
////   ff(4)
//let p16 = Let("ff",
//              FirstClassFunDeclaration("sum",["x"],BinOp(PLUS,Variable("x"),IntConstant(1))),
//              FirstClassFunCall(Variable("ff"),[IntConstant(4)]))

////// function fact(n) -> ((n == 1) ? 1 : n * fact(n-1))
//////   fact(5)
////let p17 = FunDeclaration("fact",
////                         ["n"],
////                         If(Equality(Variable("n"),IntConstant(1)),
////                            IntConstant(1),
////                            BinOp(TIMES,Variable("n"),FunCall("fact",[BinOp(MINUS,Variable("n"),IntConstant(1))]))),
////                         FunCall("fact",[IntConstant(5)]))

////// let ff = fun fact(x) -> x*(x-1) in
//////   ff(4)
////let p18 = Let("ff",
////              FirstClassFunDeclaration("fact",["n"],
////                                       If(Equality(Variable("n"),IntConstant(1)),
////                                          IntConstant(1),
////                                          BinOp(TIMES,Variable("n"),FirstClassFunCall(Variable("fact"),[BinOp(MINUS,Variable("n"),IntConstant(1))])))),
////              FirstClassFunCall(Variable("ff"),[IntConstant(4)]))
