module EvolvingDesignKeepingItClean.CalculateShippingCostModule

type Address = {
    Street: string
    City: string
    State: string
    ZipCode: string
    Country: string
}

type ValidatedOrder = {
    ShippingAddress: Address
}

let calculateShippingCostLegacy validatedOrder =
    let shippingAddress = validatedOrder.ShippingAddress
    if shippingAddress.Country = "US" then
        match shippingAddress.State with
        | "CA" | "OR" | "AZ" | "NV" ->
            5.0
        | _ ->
            10.0
    else
        20.0

let (|UsLocalState|UsRemoteState|International|) address =
    if address.Country = "US" then
        match address.State with
        | "CA" | "OR" | "AZ" | "NV" ->
            UsLocalState
        | _ ->
            UsRemoteState
    else
        International

let calculateShippingCost validatedOrder =
    match validatedOrder.ShippingAddress with
    | UsLocalState -> 5.0
    | UsRemoteState -> 10.0
    | International -> 20.0

type AddShippingInfoToOrder = PricedOrder -> PricedOrderWithShippingInfo
