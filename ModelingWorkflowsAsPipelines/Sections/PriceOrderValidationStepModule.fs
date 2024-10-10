module ModelingWorkflowsAsPipelines.Sections.PriceOrderValidationStepModule

open Model
open PricedOrderModule
open ValidateOrderStepModule

type GetProductPrice = ProductCode -> Price 

type PriceOrder =
    GetProductPrice
        -> ValidatedOrder
        -> PricedOrder