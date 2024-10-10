module ModelingWorkflowsAsPipelines.Sections.ValidateOrderStepModule

open Model
open CommandsModule
open ModelingWorkflowsAsPipelines.Sections.Model

type CheckProductCodeExists = ProductCode -> bool

type CheckedAddress = CheckedAddress of UnvalidatedAddress
type AddressValidationError = AddressValidationError of string
type CheckAddressExists =
    UnvalidatedAddress
        -> Result<CheckedAddress, AddressValidationError>

type ValidatedOrderLine = ValidatedOrderLine of UnvalidatedOrderLine

type ValidatedOrder =
    { OrderId: OrderId
      CustomerInfo: CustomerInfo
      ShippingAddress: Address
      BillingAddress: Address
      OrderLines: ValidatedOrderLine list }

type ValidationError = ValidationError of AddressValidationError

type ValidateOrder =
    CheckProductCodeExists
        -> CheckAddressExists
        -> UnvalidatedOrder
        -> Result<ValidatedOrder, ValidationError>