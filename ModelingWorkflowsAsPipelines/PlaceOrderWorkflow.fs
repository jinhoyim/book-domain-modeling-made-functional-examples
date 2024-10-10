module PlaceOrderWorkflow

open DomainApi

// -----------------------------
// Order life cycle
// -----------------------------

// validated state
type ValidatedOrderLine = { ProductCode: string; Quantity: int }

type ValidatedOrder =
    { OrderId: OrderId
      CustomerInfo: CustomerInfo
      ShippingAddress: Address
      BillingAddress: Address
      OrderLines: ValidatedOrderLine list }

// priced state
type PricedOrderLine =
    { ProductCode: string
      Quantity: int
      Price: decimal }

type PricedOrder =
    { OrderId: OrderId
      CustomerInfo: CustomerInfo
      ShippingAddress: Address
      BillingAddress: Address
      OrderLines: PricedOrderLine list
      AmountToBill: BillingAmount }

// all states combined
type Order =
    | Unvalidated of UnvalidatedOrder
    | Validated of ValidatedOrder
    | Priced of PricedOrder

// -----------------------------
// Definitions of Internal Steps
// -----------------------------

// Step 1: ValidateOrder

// services used by ValidateOrder

type CheckProductCodeExists = ProductCode -> bool
type AddressValidationError = AddressValidationError of string
type CheckedAddress = CheckedAddress of UnvalidatedAddress
type CheckAddressExists = UnvalidatedAddress -> AsyncResult<CheckedAddress, AddressValidationError>

type ValidateOrder =
    CheckProductCodeExists
        -> CheckAddressExists
        -> UnvalidatedOrder
        -> AsyncResult<ValidatedOrder, ValidationError list>

// Step 2: PriceOrder

// services used by PriceOrder
type GetProductPrice = ProductCode -> Price
type PricingError = PricingError of string
type PriceOrder = GetProductPrice -> ValidatedOrder -> Result<PricedOrder, PricingError>
