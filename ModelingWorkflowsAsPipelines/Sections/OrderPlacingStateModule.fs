module ModelingWorkflowsAsPipelines.Sections.OrderPlacingStateModule

open Model
open CommandsModule
open ValidateOrderStepModule
open PricedOrderModule

type Order =
    | Unvalidated of UnvalidatedOrder
    | Validated of ValidatedOrder
    | Priced of PricedOrder