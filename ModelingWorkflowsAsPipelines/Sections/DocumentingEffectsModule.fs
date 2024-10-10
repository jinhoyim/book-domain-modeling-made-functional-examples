module ModelingWorkflowsAsPipelines.Sections.DocumentingEffectsModule

open Model
open CommandsModule
open PricedOrderModule

type AsyncResult<'success, 'failure> = Async<Result<'success, 'failure>>
    
type ProductCode = ProductCode of string
type CheckProductCodeExists = ProductCode -> bool

type CheckedAddress = CheckedAddress of UnvalidatedAddress
type AddressValidationError = AddressValidationError of string
type CheckAddressExists =
    UnvalidatedAddress
        -> AsyncResult<CheckedAddress, AddressValidationError>

type ValidatedOrderLine = { ProductCode: string; Quantity: int }

type ValidatedOrder =
    { OrderId: OrderId
      CustomerInfo: CustomerInfo
      ShippingAddress: Address
      BillingAddress: Address
      OrderLines: ValidatedOrderLine list }

type ValidationError = ValidationError of AddressValidationError

// Async 의 전염
type ValidateOrder =
    CheckProductCodeExists
        -> CheckAddressExists
        -> UnvalidatedOrder
        -> AsyncResult<ValidatedOrder, ValidationError list>

type Price = Price of decimal
type PricingError = PricingError of string
type GetProductPrice = ProductCode -> Price
type PriceOrder =
    GetProductPrice
        -> ValidatedOrder
        -> Result<PricedOrder, PricingError>

type EmailAddress = EmailAddress of string
type HtmlString = HtmlString of string
type SendResult = Sent | NotSent
type OrderAcknowledgement = {
    EmailAddress: EmailAddress
    Letter: HtmlString
}

type SendOrderAcknowledgement = OrderAcknowledgement -> Async<SendResult>
type CreateOrderAcknowledgementLetter = PricedOrder -> HtmlString
type OrderAcknowledgementSent = {
    OrderId: OrderId
    EmailAddress: EmailAddress
}

type AcknowledgeOrder =
    CreateOrderAcknowledgementLetter
        -> SendOrderAcknowledgement
        -> PricedOrder
        -> Async<OrderAcknowledgementSent option>