module WorkingWithErrors.FruitErrorModule

let bind switchFn =
    fun twoTrackInput ->
        match twoTrackInput with
        | Ok success -> switchFn success
        | Error failure -> Error failure

// 1트랙 입력을 2트랙 입력으로 변경해서 입력 함수의 2트랙 출력에 연결
let bind2 switchFn twoTrackInput =
    match twoTrackInput with
    | Ok success -> switchFn success
    | Error failure -> Error failure
   
// 2트랙 출력에 연결할 1트랙 함수를 2트랙 함수로 변환하고 2트랙 출력으로 변환
let map f aResult =
    match aResult with
    | Ok success -> Ok (f success)
    | Error failure -> Error failure

let mapError f aResult =
    match aResult with
    | Ok success -> Ok success
    | Error failure -> Error (f failure)
    
type AppleError = AppleError of string
type BananaError = BananaError of string
type CherryError = CherryError of string
    
type FruitError =
    | AppleErrorCase of AppleError
    | BananaErrorCase of BananaError
    | CherryErrorCase of CherryError

type Apple = Apple of string
type Banana = Banana of string
type Cherry = Cherry of string
type Lemon = Lemon of string

type FunctionA = Apple -> Result<Banana, AppleError>
type FunctionB = Banana -> Result<Cherry, BananaError>
type FunctionC = Cherry -> Result<Lemon, CherryError>

// let functionA: FunctionA = fun (Apple name) -> Ok (Banana name)
let functionA: FunctionA = fun (Apple name) -> Error (AppleError "apple error")
let functionB: FunctionB = fun (Banana name) -> Ok (Cherry name)
let functionC: FunctionC = fun (Cherry name) -> Ok (Lemon name)

let functionAFruitError input =
    input
    |> functionA
    |> mapError AppleErrorCase
let functionBFruitError input =
    input
    |> functionB
    |> mapError BananaErrorCase
let functionCFruitError input =
    input
    |> functionC
    |> mapError CherryErrorCase
let functionABC input =
    input
    |> functionAFruitError
    |> bind2 functionBFruitError
    |> bind2 functionCFruitError

let result = functionABC (Apple "Apple")
match result with
| Ok (Lemon name) -> printfn $"Success: %s{name}"
| Error (AppleErrorCase (AppleError msg)) -> printfn $"Error: %A{msg}"
| Error (BananaErrorCase (BananaError msg)) -> printfn $"Error: %A{msg}"
| Error (CherryErrorCase (CherryError msg)) -> printfn $"Error: %A{msg}"


