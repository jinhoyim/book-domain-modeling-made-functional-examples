module ModelingWorkflowsAsPipelines.Sections.Model

type Address = {
    Street: string
    City: string
    State: string
    Zip: string
}

type EmailAddress = EmailAddress of string

type CustomerInfo = {
    FirstName: string
    LastName: string
    Email: EmailAddress
}

type OrderId = OrderId of string

type ProductCode = ProductCode of string

type Price = Price of decimal

type UnvalidatedCustomerInfo = UnvalidatedCustomerInfo of CustomerInfo
type UnvalidatedAddress = UnvalidatedAddress of Address
type UnvalidatedOrderLine = { ProductCode: string; Quantity: int }

type PricedOrderLine = {
    ProductCode: string
    Quantity: int
    Price: decimal
}

type BillingAmount = BillingAmount of decimal