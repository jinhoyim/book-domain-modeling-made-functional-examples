type UnvalidatedCustomerInfo = {
    FirstName: string
    LastName: string
    Email: string
}

type UnvalidatedAddress = {
    AddressLine1: string
    AddressLine2: string
    City: string
    State: string
    ZipCode: string
}
type CheckedAddress = CheckedAddress of UnvalidatedAddress
type UnvalidatedOrderLine = {
    OrderLineId: string
    ProductCode: string
    Quantity: decimal
}

type UnvalidatedOrder = {
    OrderId: string
    CustomerInfo: UnvalidatedCustomerInfo
    ShippingAddress: UnvalidatedAddress
    Lines: UnvalidatedOrderLine list
}

type OrderLineId = OrderLineId of string
module OrderLineId =
    let create str =
        if System.String.IsNullOrEmpty str then
            failwith "OrderLineId must not be null or empty"
        elif str.Length > 50 then
            failwith "OrderLineId must not be longer than 50 characters"
        else
            OrderLineId str
    let value (OrderLineId str) = str

type ProductCode =
        | Widget of string
        | Gizmo of string
module ProductCode =
    let create str =
        match str with
        | s when System.String.IsNullOrEmpty s ->
            failwith "Product code must not be null or empty"
        | s when s.Length > 50 ->
            failwith "Product code must not be longer than 50 characters"
        | s when s.StartsWith("W") ->
            Widget s
        | s when s.StartsWith("G") ->
            Gizmo s
        | _ -> failwith "Invalid product code"
    let value productCode = 
        match productCode with
        | Widget s -> s
        | Gizmo s -> s

type UnitQuantity = UnitQuantity of int
module UnitQuantity =
    let create qty =
        if qty < 1 then
            failwith "Quantity must be greater than 0"
        else
            UnitQuantity qty
    let value (UnitQuantity qty) = qty
        
type KilogramQuantity = KilogramQuantity of decimal
module KilogramQuantity =
    let create qty =
        if qty <= 0.0m then
            failwith "Quantity must be greater than 0"
        else
            KilogramQuantity qty
    let value (KilogramQuantity qty) = qty

type OrderQuantity =
        | Unit of UnitQuantity
        | Kilogram of KilogramQuantity
module OrderQuantity =
    let value qty =
        match qty with
            | Unit uq ->
                uq |> UnitQuantity.value |> decimal
            | Kilogram kq ->
                kq |> KilogramQuantity.value

type OrderId = private OrderId of string
module OrderId =
    let create str =
        if System.String.IsNullOrEmpty str then
            failwith "OrderId must not be null or empty"
        elif str.Length > 50 then
            failwith "OrderId must not be longer than 50 characters"
        else
            OrderId str
    let value (OrderId str) = str

type String50 = String50 of string
module String50 =
    let create str =
        if System.String.IsNullOrEmpty str then
            failwith "String must not be null or empty"
        elif str.Length > 50 then
            failwith "String must not be longer than 50 characters"
        else
            String50 str
    let value (String50 str) = str
            
    let createOption str =
        if System.String.IsNullOrEmpty str then
            None
        elif str.Length > 50 then
            failwith "String must not be longer than 50 characters"
        else
            Some (String50 str)
    
type ZipCode = ZipCode of string
module ZipCode =
    let create str =
        if System.String.IsNullOrEmpty str then
            failwith "Zip code must not be null or empty"
        elif str.Length > 10 then
            failwith "Zip code must not be longer than 10 characters"
        else
            ZipCode str
    let value (ZipCode str) = str


type ValidatedOrderLine = {
    OrderLineId: OrderLineId
    ProductCode: ProductCode
    Quantity: OrderQuantity
}

type EmailAddress = EmailAddress of string
module EmailAddress =
    let create str =
        if System.String.IsNullOrEmpty str then
            failwith "Email address must not be null or empty"
        elif str.Length > 50 then
            failwith "Email address must not be longer than 50 characters"
        else
            EmailAddress str
    let value (EmailAddress str) = str

type PersonalName = {
    FirstName: String50
    LastName: String50
}
type CustomerInfo = {
    Name: PersonalName
    EmailAddress: EmailAddress
}

type Address = {
    AddressLine1: String50
    AddressLine2: String50 option
    City: String50
    State: String50
    ZipCode: ZipCode
}

type ValidatedOrder = {
    OrderId: OrderId
    CustomerInfo: CustomerInfo
    ShippingAddress: Address
    BillingAddress: Address
    OrderLines: ValidatedOrderLine list
}

type Price = private Price of decimal
module Price =
    let create price =
        if price <= 0.0m then
            failwith "Price must be greater than 0"
        else
            Price price
    let value (Price price) = price
    let multiply qty (Price price) =
        create (qty * price)

type BillingAmount = BillingAmount of decimal
module BillingAmount =
    let create amount =
        match amount with
            | a when a <= 0.0m ->
                failwith "Amount must be greater than 0"
            | a when a > 1000000.0m ->
                failwith "Amount must not be greater than 1,000,000"
            | _ -> BillingAmount amount
    let sumPrices prices =
        prices
        |> List.map (fun (Price price) -> price)
        |> List.sum
        |> create
    let value (BillingAmount sumPrices) = sumPrices

type PricedOrderLine = {
    OrderLineId: OrderLineId
    ProductCode: ProductCode
    Quantity: OrderQuantity
    LinePrice: Price
}



type PricedOrder = {
    OrderId: OrderId
    CustomerInfo: CustomerInfo
    ShippingAddress: Address
    BillingAddress: Address
    Lines: PricedOrderLine list
    AmountToBill: BillingAmount
}

type OrderPlaced = PricedOrder

type OrderAcknowledgementSent = {
    OrderId: OrderId
    EmailAddress: EmailAddress
}

type BillableOrderPlaced = {
    OrderId: OrderId
    BillingAddress: Address
    AmountToBill: BillingAmount
}

type PlaceOrderEvent =
    | OrderPlaced of OrderPlaced
    | BillableOrderPlaced of BillableOrderPlaced
    | AcknowledgementSent of OrderAcknowledgementSent


type ValidationError = {
  FieldName: string
  ErrorDescription: string
}

type PricingError = {
    FieldName: string
    ErrorDescription: string
}

type ServiceInfo = { Name: string; Endpoint: string }
type RemoteServiceError = {
    Service: ServiceInfo
    Exception: System.Exception
}

type AuthorizationException(message: string) =
    inherit System.Exception(message)

type PlaceOrderError =
    | Validation of ValidationError
    | Pricing of PricingError
    | RemoteService of RemoteServiceError

type ValidateOrder =
    UnvalidatedOrder
        -> Result<ValidatedOrder, ValidationError>

type PriceOrder =
    ValidatedOrder
        -> Result<PricedOrder, PricingError>

type AcknowledgeOrder =
    PricedOrder
        -> OrderAcknowledgementSent option
        
    
type CreateEvents =
    PricedOrder
        -> OrderAcknowledgementSent option
        -> PlaceOrderEvent list

let validateOrder: ValidateOrder =
    fun unvalidatedOrder ->
        if System.String.IsNullOrEmpty unvalidatedOrder.OrderId then
            Error {
                FieldName = "OrderId"
                ErrorDescription = "OrderId must not be null or empty"
            }
        else
            Ok {
                OrderId = OrderId.create unvalidatedOrder.OrderId
                CustomerInfo = {
                    Name = {
                        FirstName = String50.create unvalidatedOrder.CustomerInfo.FirstName
                        LastName = String50.create unvalidatedOrder.CustomerInfo.LastName
                    }
                    EmailAddress = EmailAddress.create unvalidatedOrder.CustomerInfo.Email
                }
                ShippingAddress = {
                    AddressLine1 = String50.create unvalidatedOrder.ShippingAddress.AddressLine1
                    AddressLine2 = unvalidatedOrder.ShippingAddress.AddressLine2 |> String50.createOption
                    City = String50.create unvalidatedOrder.ShippingAddress.City
                    State = String50.create unvalidatedOrder.ShippingAddress.State
                    ZipCode = ZipCode.create unvalidatedOrder.ShippingAddress.ZipCode
                }
                BillingAddress = {
                    AddressLine1 = String50.create unvalidatedOrder.ShippingAddress.AddressLine1
                    AddressLine2 = unvalidatedOrder.ShippingAddress.AddressLine2 |> String50.createOption
                    City = String50.create unvalidatedOrder.ShippingAddress.City
                    State = String50.create unvalidatedOrder.ShippingAddress.State
                    ZipCode = ZipCode.create unvalidatedOrder.ShippingAddress.ZipCode
                }
                OrderLines = unvalidatedOrder.Lines
                    |> List.map (fun line ->
                        {
                            OrderLineId = OrderLineId.create line.OrderLineId
                            ProductCode = ProductCode.create line.ProductCode
                            Quantity = 
                                match line.Quantity with
                                | qty when qty < 1.0m -> failwith "Quantity must be greater than 0"
                                | qty when qty % 1.0m = 0.0m -> Unit (UnitQuantity (int qty))
                                | qty -> Kilogram (KilogramQuantity qty)
                        }
                    )
            }

let priceOrder: PriceOrder =
    fun validatedOrder ->
        let getProductPrice (productCode) =
            match productCode with
            | Widget _ -> Price.create 10.0m
            | Gizmo _ -> Price.create 20.0m
        let totalAmount =
            validatedOrder.OrderLines
            |> List.sumBy (fun line ->
                match line.Quantity with
                | Unit (UnitQuantity qty) -> Price.value (Price.multiply (decimal qty) (getProductPrice line.ProductCode))
                | Kilogram (KilogramQuantity qty) -> Price.value (Price.multiply qty (getProductPrice line.ProductCode))
            )
        Ok {
            OrderId = validatedOrder.OrderId
            CustomerInfo = validatedOrder.CustomerInfo
            ShippingAddress = validatedOrder.ShippingAddress
            BillingAddress = validatedOrder.BillingAddress
            Lines = validatedOrder.OrderLines
                |> List.map (fun line ->
                    {
                        OrderLineId = line.OrderLineId
                        ProductCode = line.ProductCode
                        Quantity = line.Quantity
                        LinePrice = 
                            match line.Quantity with
                            | Unit (UnitQuantity qty) -> Price.multiply (decimal qty) (getProductPrice line.ProductCode)
                            | Kilogram (KilogramQuantity qty) -> Price.multiply qty (getProductPrice line.ProductCode)
                    }
                )
            AmountToBill = BillingAmount.create totalAmount
        }

let acknowledgeOrder: AcknowledgeOrder =
    fun pricedOrder ->
        Some {
            OrderId = pricedOrder.OrderId
            EmailAddress = pricedOrder.CustomerInfo.EmailAddress
        }
let createOrderPlacedEvent (placedOrder:PricedOrder) = placedOrder
let listOfOption opt =
    match opt with
    | Some x -> [x]
    | None -> []

let createBillingEvent (placedOrder: PricedOrder): BillableOrderPlaced option =
    let billingAmount = placedOrder.AmountToBill |> BillingAmount.value
    if billingAmount > 0.0m then
        let order = {
            OrderId = placedOrder.OrderId
            BillingAddress = placedOrder.BillingAddress
            AmountToBill = placedOrder.AmountToBill
        }
        Some order
    else
        None
        
let createEvents: CreateEvents =
    fun pricedOrder acknowledgementEventOpt ->
        let event1 =
            pricedOrder
            |> createOrderPlacedEvent
            |> PlaceOrderEvent.OrderPlaced
            |> List.singleton
        let event2 =
            acknowledgementEventOpt
            |> Option.map PlaceOrderEvent.AcknowledgementSent
            |> listOfOption
        let event3 =
            pricedOrder
            |> createBillingEvent
            |> Option.map PlaceOrderEvent.BillableOrderPlaced
            |> listOfOption
        
        [
            yield! event1
            yield! event2
            yield! event3
        ]

let bind switchFn twoTrackInput =
    match twoTrackInput with
    | Ok success -> switchFn success
    | Error failure -> Error failure

let map f aResult =
    match aResult with
    | Ok success -> Ok (f success)
    | Error failure -> Error failure
    
let mapError f aResult =
    match aResult with
    | Ok success -> Ok success
    | Error failure -> Error (f failure)
    
let serviceExceptionAdapter serviceInfo serviceFn x =
    try
        Ok (serviceFn x)
    with
    | :? System.TimeoutException as ex ->
        Error { Service=serviceInfo; Exception=ex }
    | :? AuthorizationException as ex ->
        Error { Service=serviceInfo; Exception=ex}

let serviceInfo = { Name="CustomerService"; Endpoint="http://localhost:8080" }
let checkAddressExists (address: UnvalidatedAddress): CheckedAddress =
    let checkedAddress = CheckedAddress {
        AddressLine1 = address.AddressLine1
        AddressLine2 = address.AddressLine2
        City = address.City
        State = address.State
        ZipCode = address.ZipCode
    }
    checkedAddress

let checkAddressExistsR address =
    let adaptedService =
        serviceExceptionAdapter serviceInfo checkAddressExists
    address
    |> adaptedService
    |> mapError RemoteService

let logError msg =
    printfn $"ERROR %s{msg}"

let tee f x =
    f x
    x

let adaptDeadEnd f =
    map (tee f)
    
let validateOrderAdapted unvalidatedOrder =
    unvalidatedOrder
    |> validateOrder
    |> mapError Validation
    
let priceOrderAdapted validatedOrder =
    validatedOrder
    |> priceOrder
    |> mapError Pricing

// let placeOrder unvalidatedOrder =
//     
//     unvalidatedOrder
//     |> validateOrderAdapted
//     |> bind priceOrderAdapted
//     |> map acknowledgeOrder
//     |> map createEvents

type ResultBuilder() =
    member this.Return(x) = Ok x
    member this.Bind(x, f) = bind f x
let result = ResultBuilder()

let placeOrder unvalidatedOrder =
    result {
        let! validatedOrder =
            validateOrder unvalidatedOrder
            |> mapError PlaceOrderError.Validation
        let! pricedOrder =
            priceOrder validatedOrder
            |> mapError PlaceOrderError.Pricing
        let acknowledgementEventOpt =
            acknowledgeOrder pricedOrder
        let events =
            createEvents pricedOrder acknowledgementEventOpt
        return events
    }
    
let compsingComputationExpressions unvalidatedOrder =
    let validateOrder unvalidatedOrder = result {
        let! validatedOrder =
            validateOrder unvalidatedOrder
            |> mapError PlaceOrderError.Validation
        return validatedOrder
    }
    
    let priceOrder validateOrder = result {
        let! pricedOrder =
            priceOrder validateOrder
            |> mapError PlaceOrderError.Pricing
        return pricedOrder
    }
    
    let placeOrder unvalidatedOrder = result {
        let! validatedOrder = validateOrder unvalidatedOrder
        let! pricedOrder = priceOrder validatedOrder
        return pricedOrder
    }
    placeOrder unvalidatedOrder
    
let prepend firstR restR =
    match firstR, restR with
    | Ok first, Ok rest -> Ok (first::rest)
    | Error err1, Ok _ -> Error err1
    | Ok _, Error err2 -> Error err2
    | Error err1, Error _ -> Error err1

let sequence aListOfResults =
    let initialValue = Ok []
    List.foldBack prepend aListOfResults initialValue
    
type IntOrError = Result<int, string>
let listOfSuccesses: IntOrError list = [Ok 1; Ok 2; Ok 3]
let successResult = sequence listOfSuccesses // Ok [1; 2; 3]
let listOfFailures: IntOrError list = [Ok 1; Error "error"; Ok 3]
let failureResult = sequence listOfFailures // Error "error"
printfn $"Success: %A{successResult}"
printfn $"Failure: %A{failureResult}"
