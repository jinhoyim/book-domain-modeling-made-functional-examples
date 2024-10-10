module InputFunction =
    let evalWith5ThenAdd2 f = f (5) + 2
    let evalResult = evalWith5ThenAdd2 (fun x -> x * 2)
    printfn $"%A{evalResult}"


module FunctionComposition =
    let adderGenerator numberToAdd =
        // fun x -> x + numberToAdd
        let innerFn x = x + numberToAdd
        innerFn

    let adder1 = adderGenerator 1
    let adder2 = adderGenerator 2
    let addResult = adder1 (adder2 (3))
    printfn $"%A{addResult}"
    let addResult2 = adderGenerator 3 2
    printfn $"%A{addResult2}"

module Currying =
    let add x y = x + y
    let curriedAdd = add 1 2
    printfn $"%A{curriedAdd}"
    let curriedAdd2 = add (1) (2)
    printfn $"%A{curriedAdd2}"

    let add2 (x, y) = x + y
    let curriedAdd3 = add2 (1, 2)
    printfn $"%A{curriedAdd3}"

module PartialApplication =
    let sayGreeting greeting name = $"{greeting}, {name}!"
    let sayHello = sayGreeting "Hello"
    let sayGoodbye = sayGreeting "Goodbye"
    printfn "%A" (sayHello "World")
    printfn "%A" (sayGoodbye "World")

module HandleException =
    // 함수형 프로그래밍에서 예외는 잘 사용하지 않지만 알아두기
    let twelveDivideBy n =
        match n with
        | 6 -> 2
        | 1 -> 12
        | 0 -> failwith "Cannot divide by zero"

    let twelveDivideBy6 = twelveDivideBy 6
    printfn $"%A{twelveDivideBy6}"
    let twelveDivideBy1 = twelveDivideBy 1
    printfn $"%A{twelveDivideBy1}"

    // handle exception
    try
        let twelveDivideBy0 = twelveDivideBy 0
        printfn $"%A{twelveDivideBy0}"
    with
    | :? System.DivideByZeroException as ex -> printfn $"%A{ex.Message}"
    | ex -> printfn $"%A{ex.Message}"

module NonZeroInteger =
    type NonZeroInt = private NonZeroInt of int

    let create n =
        if n > 0 then
            Ok(NonZeroInt n)
        else
            Error "Value must be greater than zero."
            
    let createWithPatternMatching n =
        match n with
        | n when n > 0 -> Ok(NonZeroInt n)
        | _ -> Error "Value must be greater than zero."

    let value (NonZeroInt n) = n

module UsingNonZeroInteger =
    open NonZeroInteger
    
    let twelveDivideBy (n: NonZeroInt) =
        match value n with
        | 6 -> 2
        | 1 -> 12
        | i -> failwith "todo"
    
    let input6 = create 6
    match input6 with
    | Ok n ->
        let twelveDivideBy6 = twelveDivideBy n
        printfn $"%A{twelveDivideBy6}"
    | Error msg -> printfn $"%A{msg}"
    
    let input0 = create 0
    match input0 with
    | Ok n ->
        let twelveDivideBy0 = twelveDivideBy n
        printfn $"%A{twelveDivideBy0}"
    | Error
        msg -> printfn $"%A{msg}"

module UsingNoneOutput =
    let twelveDivideBy n =
        match n with
        | 6 -> Some 2
        | 1 -> Some 12
        | 0 -> None
    
    let twelveDivideBy6 = twelveDivideBy 6
    match twelveDivideBy6 with
    | Some result -> printfn $"%A{result}"
    | None -> printfn "None"
    
    let twelveDivideBy0 = twelveDivideBy 0
    match twelveDivideBy0 with
    | Some result -> printfn $"%A{result}"
    | None -> printfn "None"

module CompositionOfFunctions =
    let add1 x = x + 1
    let square x = x * x
    
    let add1ThenSquare = add1 >> square
    printfn $"%A{add1ThenSquare 3}"
    
    let add1ThenSquare2 = square << add1
    printfn $"%A{add1ThenSquare2 3}"
    
    let add1ThenSquare3 x = x |> add1 |> square 
    printfn $"%A{add1ThenSquare3 3}"

    let isEven x = x % 2 = 0
    let printBool x = sprintf $"%b{x}"
    let isEvenThenPrint x = x |> isEven |> printBool
    printfn $"{isEvenThenPrint 2}"
    
    let printfnBool x = printfn $"%b{x}"
    let isEvenThenPrint2 x = x |> isEven |> printfnBool
    isEvenThenPrint2 2
    
    let printOption x =
        match x with
        | Some i -> printfn $"%A{i}"
        | None -> printfn "None"
    5 |> add1 |> Some |> printOption
    