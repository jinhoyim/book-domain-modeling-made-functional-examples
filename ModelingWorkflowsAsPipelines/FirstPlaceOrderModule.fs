module ModelingWorkflowsAsPipelines.Sections.FirstPlaceOrderModule

type UnvalidatedCustomerInfo = {
    FirstName: string
    LastName: string
    Email: string
}

type UnvalidatedAddress = {
    Street: string
    City: string
    State: string
    Zip: string
}

type UnvalidatedOrder = {
    OrderId: string
    CustomerInfo: UnvalidatedCustomerInfo
    ShippingAddress: UnvalidatedAddress
}

// Order-Placing Workflow를 시작하는 커맨드
type PlaceOrder = {
    OrderForm: UnvalidatedOrder
    Timestamp: System.DateTime
    UserId: string
}
