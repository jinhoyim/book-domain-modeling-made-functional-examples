module DomainApi

type EmailAddress = EmailAddress of string
type CustomerInfo =
    { FirstName: string
      LastName: string
      Email: EmailAddress }
type Address =
    { Street: string
      City: string
      State: string
      Zip: string }
    
type OrderId = OrderId of string
type ProductCode = ProductCode of string
type Price = Price of decimal
type BillingAmount = BillingAmount of decimal

// -----------------------------
// Input data region
// -----------------------------
type UnvalidatedOrder =
    { OrderId: string
      CustomerInfo: UnvalidatedCustomerInfo
      ShippingAddress: UnvalidatedAddress }

and UnvalidatedCustomerInfo = { Name: string; Email: string }

and UnvalidatedAddress =
    { Street: string
      City: string
      State: string
      Zip: string }

// -----------------------------
// Input Command
// -----------------------------
type Command<'data> =
    { Data: 'data
      Timestamp: System.DateTime
      UserId: string }

type PlaceOrderCommand = Command<UnvalidatedOrder>

// -----------------------------
// Public API
// -----------------------------

// Success output of PlaceOrder workflow
type OrderPlaced =
    { OrderId: OrderId
      CustomerInfo: CustomerInfo
      ShippingAddress: Address }

type BillableOrderPlaced =
    { OrderId: OrderId
      BillingAddress: Address
      AmountToBill: BillingAmount }

type OrderAcknowledgementSent =
    { OrderId: OrderId
      EmailAddress: EmailAddress }

type PlaceOrderEvent =
    | OrderPlaced of OrderPlaced
    | BillableOrderPlaced of BillableOrderPlaced
    | AcknowledgementSent of OrderAcknowledgementSent

// Failure output of PlaceOrder workflow
type PlaceOrderError = ValidationError of ValidationError list

and ValidationError =
    { FieldName: string
      ErrorDescription: string }

type AsyncResult<'success, 'failure> = Async<Result<'success, 'failure>>
type PlaceOrderWorkflow = PlaceOrderCommand -> AsyncResult<PlaceOrderEvent list, PlaceOrderError>
