module ModelingWorkflowsAsPipelines.Sections.PricedOrderModule

open Model

type PricedOrder = {
      OrderId: OrderId
      CustomerInfo: CustomerInfo
      ShippingAddress: Address
      BillingAddress: Address
      OrderLines: PricedOrderLine list
      AmountToBill: BillingAmount
}