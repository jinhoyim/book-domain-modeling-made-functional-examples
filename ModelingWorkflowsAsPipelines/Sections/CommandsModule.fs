module ModelingWorkflowsAsPipelines.Sections.CommandsModule

open Model

// 커맨드의 공통 필드를 정의한 상위 타입
type Command<'data> = {
    Data: 'data
    Timestamp: System.DateTime
    UserId: string
}

type UnvalidatedOrder = {
    OrderId: OrderId
    CustomerInfo: UnvalidatedCustomerInfo
    ShippingAddress: UnvalidatedAddress
    BillingAddress: UnvalidatedAddress
    OrderLines: UnvalidatedOrderLine list
}

type PlaceOrder = Command<UnvalidatedOrder>
type ChangeOrder = Command<string>
type CancelOrder = Command<string>

type OrderTakingCommand =
    | Place of PlaceOrder
    | Change of ChangeOrder
    | Cancel of CancelOrder