open System

module UnitQuantity =
    type UnitQuantity = private UnitQuantity of int
    let value (UnitQuantity qty) = qty

    let create qty =
        if qty < 1 then
            Error "UnitQuantity can not be negative"
        else if qty > 1000 then
            Error "UnitQuantity can not be more than 1000"
        else
            Ok (UnitQuantity qty)

let exampleUnitQuantity () =
    let unitQuantity = UnitQuantity.create 10
    
    match unitQuantity with
    | Error msg -> printfn $"Failure, Message is %s{msg}"
    | Ok quantity ->
        printfn $"Success, Value is %A{quantity}"
        let innerValue = UnitQuantity.value quantity
        printfn $"innerValue is %i{innerValue}"

exampleUnitQuantity ()

module UnitsOfMeasureModule =
    [<Measure>]
    type kg
    type KilogramQuantity = KilogramQuantity of decimal<kg>
    let fiveKg = KilogramQuantity 5.0m<kg>
    let (KilogramQuantity fiveKgValue) = fiveKg
    printfn $"fiveKg is %A{fiveKg}"
    printfn $"fiveKgValue is %M{fiveKgValue}"

module InvariantTypeModule =
    type NonEmptyList<'a> = { First: 'a; Rest: 'a list }

    type OrderLine = {
        OrderLineId: string
    }
    
    type Order = {
        OrderLines: NonEmptyList<OrderLine>
    }
    
    let order = {
        OrderLines = {
            First = { OrderLineId = "1" }
            Rest = [{ OrderLineId = "2" }]
        }
    }
    
    let order2 = {
        OrderLines = {
            First = { OrderLineId = "1" }
            Rest = []
        }
    }

module BusinessRulesInTypeSystem =
    type EmailAddress = EmailAddress of string
    type VerifiedEmailAddress = VerifiedEmailAddress of string
    type CustomerEmail =
        | Unverified of EmailAddress
        | Verified of VerifiedEmailAddress

    type EmailContactInfo = EmailContactInfo of string
    type PostalContactInfo = PostalContactInfo of string
    type BothContactMethods = {
        Email: EmailContactInfo
        Address: PostalContactInfo
    }
    type ContactInfo =
        | EmailOnly of EmailContactInfo
        | PostalOnly of PostalContactInfo
        | EmailAndPostal of BothContactMethods
    type Name = Name of string
    type Contact = {
        Name: Name
        ContactInfo: ContactInfo
    }
    
module ConsistencyOrderAmountToBillModule =
    type Price = Price of decimal
    type OrderLineId = OrderLineId of string
    type OrderLine = {
        OrderLineId: OrderLineId
        Price: Price
    }
    type Order = {
        OrderLines: OrderLine list
        AmountToBill: Price
    }
    let findOrderLine orderLineId orderLines =
        orderLines |> List.find (fun line -> line.OrderLineId = orderLineId)
    let replaceOrderLine orderLineId newOrderLine orderLines =
        orderLines |> List.map (fun line -> if line.OrderLineId = orderLineId then newOrderLine else line)
    let changeOrderLinePrice order orderLineId newPrice =
        let orderLine = order.OrderLines |> findOrderLine orderLineId
        let newOrderLine = { orderLine with Price = newPrice }
        let newOrderLines = order.OrderLines |> replaceOrderLine orderLineId newOrderLine
        let newAmountToBill = newOrderLines |> List.sumBy (fun line -> let (Price p) = line.Price in p)
        let newOrder = {
            order with
                OrderLines = newOrderLines
                AmountToBill = Price newAmountToBill
        }
        newOrder
    let order = {
        OrderLines = [
            { OrderLineId = OrderLineId "1"; Price = Price 10.0m }
            { OrderLineId = OrderLineId "2"; Price = Price 20.0m }
        ]
        AmountToBill = Price 30.0m
    }
    let newOrder = changeOrderLinePrice order (OrderLineId "2") (Price 25.0m)
    printfn $"newOrder is %A{newOrder}"

module MoneyTransferModule =
    type MoneyTransferId = MoneyTransferId of Guid
    type AccountId = AccountId of string
    type Money = Money of decimal
    type MoneyTransfer = {
        Id: MoneyTransferId
        ToAccount: AccountId
        FromAccount: AccountId
        Amount: Money
    }

