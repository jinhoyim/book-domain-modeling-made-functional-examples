module ModelingWorkflowsAsPipelines.Sections.AcknowledgeOrderStepModule

open Model
open PricedOrderModule

type HtmlString = HtmlString of string
type OrderAcknowledgement = {
    EmailAddress: EmailAddress
    Letter: HtmlString
}

type CreateOrderAcknowledgementLetter = PricedOrder -> HtmlString

type SendResult = Sent | NotSent
type SendOrderAcknowledgement = OrderAcknowledgement -> SendResult

type OrderAcknowledgementSent = {
    OrderId: OrderId
    EmailAddress: EmailAddress
}

type AcknowledgeOrder =
    CreateOrderAcknowledgementLetter
        -> SendOrderAcknowledgement
        -> PricedOrder
        -> OrderAcknowledgementSent

type OrderPlaced = PricedOrder
type BillableOrderPlaced = {
    OrderId: OrderId
    BillingAddress: Address
    AmountToBill: BillingAmount
}

type PlaceOrderResult = {
    OrderPlaced: OrderPlaced
    BillableOrderPlaced: BillableOrderPlaced
    OrderAcknowledgementSent: OrderAcknowledgementSent option
}

type PlaceOrderEvent =
    | OrderPlaced of OrderPlaced
    | BillableOrderPlaced of BillableOrderPlaced
    | AcknowledgementSent of OrderAcknowledgementSent
type CreateEvents =
    PricedOrder -> PlaceOrderEvent list