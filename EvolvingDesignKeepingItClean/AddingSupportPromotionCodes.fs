module EvolvingDesignKeepingItClean.AddingSupportPromotionCodes

open System.Collections.Generic

type PromotionCode = PromotionCode of string

type PricingMethod =
    | Standard
    | Promotion of PromotionCode

type ValidatedOrder = {
    PricingMethod: PricingMethod
}

type OrderDto = {
    PromotionCode: string
}

type UnvalidatedOrder = {
    PromotionCode: string
}

type ProductCode = ProductCode of string
type Price = Price of decimal

type GetProductPrice =
    ProductCode -> Price

type GetPricingFunction = PricingMethod -> GetProductPrice

type PricedOrder = {
    // OrderId : OrderId
    // CustomerInfo : CustomerInfo
    // ShippingAddress : Address
    // BillingAddress : Address
    // AmountToBill : BillingAmount
    // Lines : PricedOrderLine list
    PricingMethod : PricingMethod
    }

type PriceOrder =
    GetPricingFunction
        -> ValidatedOrder
        -> PricedOrder

type GetStandardPriceTable =
    unit -> IDictionary<ProductCode, Price>

type GetPromotionPriceTable =
    PromotionCode -> IDictionary<ProductCode, Price>

let getPricingFunction
    (standardPrices: GetStandardPriceTable)
    (promoPrices: GetPromotionPriceTable)
    : GetPricingFunction =
        let getStandardPrice: GetProductPrice =
            let standardPrices = standardPrices()
            fun productCode -> standardPrices.[productCode]
            
        let getPromotionPrice promotionCode: GetProductPrice =
            let promotionPrices = promoPrices promotionCode
            fun productCode ->
                match promotionPrices.TryGetValue productCode with
                | true, price -> price
                | false, _ -> getStandardPrice productCode
                
        fun pricingMethod ->
            match pricingMethod with
            | Standard ->
                getStandardPrice
            | Promotion promotionCode ->
                getPromotionPrice promotionCode