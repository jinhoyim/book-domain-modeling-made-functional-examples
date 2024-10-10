// Simple Type

module Domain =
    type OrderId = private OrderId of string
    module OrderId =
        let create str =
            if System.String.IsNullOrEmpty str then
                failwith "OrderId must not be null or empty"
            elif str.Length > 50 then
                failwith "OrderId must not be longer than 50 characters"
            else
                OrderId str
        let value (OrderId str) = str
    
    type OrderLineId = OrderLineId of string
    module OrderLineId =
        let create str =
            if System.String.IsNullOrEmpty str then
                failwith "OrderLineId must not be null or empty"
            elif str.Length > 50 then
                failwith "OrderLineId must not be longer than 50 characters"
            else
                OrderLineId str
        let value (OrderLineId str) = str
    
    type EmailAddress = EmailAddress of string
    module EmailAddress =
        let create str =
            if System.String.IsNullOrEmpty str then
                failwith "Email address must not be null or empty"
            elif str.Length > 50 then
                failwith "Email address must not be longer than 50 characters"
            else
                EmailAddress str
        let value (EmailAddress str) = str
    
    type UnitQuantity = UnitQuantity of int
    module UnitQuantity =
        let create qty =
            if qty < 1 then
                failwith "Quantity must be greater than 0"
            else
                UnitQuantity qty
        let value (UnitQuantity qty) = qty
        
    type KilogramQuantity = KilogramQuantity of decimal
    module KilogramQuantity =
        let create qty =
            if qty <= 0.0m then
                failwith "Quantity must be greater than 0"
            else
                KilogramQuantity qty
        let value (KilogramQuantity qty) = qty
        
    type OrderQuantity =
        | Unit of UnitQuantity
        | Kilogram of KilogramQuantity
    module OrderQuantity =
        let value qty =
            match qty with
                | Unit uq ->
                    uq |> UnitQuantity.value |> decimal
                | Kilogram kq ->
                    kq |> KilogramQuantity.value
     
    type String50 = String50 of string
    module String50 =
        let create str =
            if System.String.IsNullOrEmpty str then
                failwith "String must not be null or empty"
            elif str.Length > 50 then
                failwith "String must not be longer than 50 characters"
            else
                String50 str
        let value (String50 str) = str
                
        let createOption str =
            if System.String.IsNullOrEmpty str then
                None
            elif str.Length > 50 then
                failwith "String must not be longer than 50 characters"
            else
                Some (String50 str)
    
    type ZipCode = ZipCode of string
    module ZipCode =
        let create str =
            if System.String.IsNullOrEmpty str then
                failwith "Zip code must not be null or empty"
            elif str.Length > 10 then
                failwith "Zip code must not be longer than 10 characters"
            else
                ZipCode str
        let value (ZipCode str) = str
    
    type ProductCode =
        | Widget of string
        | Gizmo of string
    module ProductCode =
        let create str =
            match str with
            | s when System.String.IsNullOrEmpty s ->
                failwith "Product code must not be null or empty"
            | s when s.Length > 50 ->
                failwith "Product code must not be longer than 50 characters"
            | s when s.StartsWith("W") ->
                Widget s
            | s when s.StartsWith("G") ->
                Gizmo s
            | _ -> failwith "Invalid product code"
    
    type Price = private Price of decimal
    module Price =
        let create price =
            if price <= 0.0m then
                failwith "Price must be greater than 0"
            else
                Price price
        let value (Price price) = price
        let multiply qty (Price price) =
            create (qty * price)
    
    type BillingAmount = BillingAmount of decimal
    module BillingAmount =
        let create amount =
            match amount with
                | a when a <= 0.0m ->
                    failwith "Amount must be greater than 0"
                | a when a > 1000000.0m ->
                    failwith "Amount must not be greater than 1,000,000"
                | _ -> BillingAmount amount
        
        let sumPrices prices =
            prices
            |> List.map (fun (Price price) -> price)
            |> List.sum
            |> create
        let value (BillingAmount sumPrices) = sumPrices
    
    
    module WorkflowTypes =
        type UnvalidatedAddress = {
            AddressLine1: string
            AddressLine2: string
            City: string
            State: string
            ZipCode: string
        }
        type CheckedAddress = CheckedAddress of UnvalidatedAddress
        type CheckAddressExists =
            UnvalidatedAddress -> CheckedAddress
        
        type CheckProductCodeExists = ProductCode -> bool
        type UnvalidatedCustomerInfo = {
            FirstName: string
            LastName: string
            Email: string
        }
        
        type UnvalidatedOrderLine = {
            OrderLineId: string
            ProductCode: string
            Quantity: decimal
        }
        
        type UnvalidatedOrder = {
            OrderId: string
            CustomerInfo: UnvalidatedCustomerInfo
            ShippingAddress: UnvalidatedAddress
            Lines: UnvalidatedOrderLine list
        }
        
        type PersonalName = {
            FirstName: String50
            LastName: String50
        }
        type CustomerInfo = {
            Name: PersonalName
            EmailAddress: EmailAddress
        }
        type Address = {
            AddressLine1: String50
            AddressLine2: String50 option
            City: String50
            State: String50
            ZipCode: ZipCode
        }
        type ValidatedOrderLine = {
            OrderLineId: OrderLineId
            ProductCode: ProductCode
            Quantity: OrderQuantity
        }
        
        type ValidatedOrder = {
            OrderId: OrderId
            CustomerInfo: CustomerInfo
            ShippingAddress: Address
            BillingAddress: Address
            OrderLines: ValidatedOrderLine list
        }
        
        type PricedOrderLine = {
            OrderLineId: OrderLineId
            ProductCode: ProductCode
            Quantity: OrderQuantity
            LinePrice: Price
        }
        
        type PricedOrder = {
            OrderId: OrderId
            CustomerInfo: CustomerInfo
            ShippingAddress: Address
            BillingAddress: Address
            Lines: PricedOrderLine list
            AmountToBill: BillingAmount
        }
        
    module ValidateOrderModule =
        open WorkflowTypes
        
        type ValidateOrder =
            CheckProductCodeExists
                -> CheckAddressExists
                -> UnvalidatedOrder
                -> ValidatedOrder
        
        let toCustomerInfo (customer: UnvalidatedCustomerInfo): CustomerInfo =
            let firstName = customer.FirstName |> String50.create
            let lastName = customer.LastName |> String50.create
            let emailAddress = customer.Email |> EmailAddress.create
            
            let name: PersonalName = {
                FirstName = firstName
                LastName = lastName 
            }
            
            let customerInfo: CustomerInfo = {
                Name = name
                EmailAddress = emailAddress
            }
            customerInfo
            
        let toAddress (checkAddressExists: CheckAddressExists) unvalidatedAddress: Address =
            // call the remote service
            let checkedAddress = checkAddressExists unvalidatedAddress
            let (CheckedAddress checkedAddress) = checkedAddress
            
            let addressLine1 =
                checkedAddress.AddressLine1 |> String50.create
            let addressLine2 =
                checkedAddress.AddressLine2 |> String50.createOption
            let city =
                checkedAddress.City |> String50.create
            let state =
                checkedAddress.State |> String50.create
            let zipCode =
                checkedAddress.ZipCode |> ZipCode.create
            
            let address: Address = {
                AddressLine1 = addressLine1
                AddressLine2 = addressLine2
                City = city
                State = state
                ZipCode = zipCode 
            }
            address
            
        let predicateToPassthru errorMsg f x =
            if f x then x
            else failwith errorMsg
        
        let toProductCode (checkProductCodeExists: CheckProductCodeExists) productCode =
            let checkProduct productCode =
                let errorMsg = sprintf $"Invalid: %A{productCode}"
                predicateToPassthru errorMsg checkProductCodeExists productCode
            
            productCode
            |> ProductCode.create
            |> checkProduct
            
        let toOrderQuantity productCode quantity =
            match productCode with
            | Widget _ ->
                quantity // decimal to int
                |> int
                |> UnitQuantity.create // to UnitQuantity
                |> OrderQuantity.Unit // lift to OrderQuantity type
            | Gizmo _ ->
                quantity
                |> KilogramQuantity.create
                |> OrderQuantity.Kilogram
            
        let toValidatedOrderLine checkProductCodeExists (unvalidatedOrderLine: UnvalidatedOrderLine) =
            let orderLineId =
                unvalidatedOrderLine.OrderLineId
                |> OrderLineId.create
            let productCode =
                unvalidatedOrderLine.ProductCode
                |> toProductCode checkProductCodeExists
            let quantity =
                unvalidatedOrderLine.Quantity
                |> toOrderQuantity productCode
            let validatedOrderLine = {
                OrderLineId = orderLineId
                ProductCode = productCode
                Quantity = quantity
            }
            validatedOrderLine
                
        let validateOrder: ValidateOrder =
            fun checkProductCodeExists checkAddressExists unvalidatedOrder ->
                let orderId =
                    unvalidatedOrder.OrderId
                        |> OrderId.create
                
                let customerInfo =
                    unvalidatedOrder.CustomerInfo
                    |> toCustomerInfo
                
                let shippingAddress =
                    unvalidatedOrder.ShippingAddress
                    |> toAddress checkAddressExists
                
                let orderLines =
                    unvalidatedOrder.Lines
                    |> List.map (toValidatedOrderLine checkProductCodeExists)
                    
                {
                    OrderId = orderId
                    CustomerInfo = customerInfo
                    ShippingAddress = shippingAddress
                    BillingAddress = shippingAddress
                    OrderLines = orderLines
                }

    module PriceOrderModule =
        open WorkflowTypes
        
        type GetProductPrice = ProductCode -> Price
        type PriceOrder =
            GetProductPrice
                -> ValidatedOrder
                -> PricedOrder
        
        let toPricedOrderLine (getProductPrice: GetProductPrice) (line: ValidatedOrderLine): PricedOrderLine =
            let qty = line.Quantity |> OrderQuantity.value
            let price = line.ProductCode |> getProductPrice
            let linePrice = price |> Price.multiply qty
            {
                OrderLineId = line.OrderLineId
                ProductCode = line.ProductCode
                Quantity = line.Quantity
                LinePrice = linePrice 
            }
        
        let priceOrder: PriceOrder =
            fun getProductPrice validatedOrder ->
                let lines =
                    validatedOrder.OrderLines
                    |> List.map (toPricedOrderLine getProductPrice)
                let amountToBill =
                    lines
                    |> List.map (fun line -> line.LinePrice)
                    |> BillingAmount.sumPrices
                let pricedOrder: PricedOrder = {
                    OrderId = validatedOrder.OrderId
                    CustomerInfo = validatedOrder.CustomerInfo
                    ShippingAddress = validatedOrder.ShippingAddress
                    BillingAddress = validatedOrder.BillingAddress
                    Lines = lines
                    AmountToBill = amountToBill
                }
                pricedOrder
    
    module AcknowledgementModuel =
        open WorkflowTypes
        
        type HtmlString = HtmlString of string
        type CreateOrderAcknowledgementLetter =
            PricedOrder -> HtmlString
        
        type OrderAcknowledgement = {
            EmailAddress: EmailAddress
            Letter: HtmlString
        }
        
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
                -> OrderAcknowledgementSent option
        
        let acknowledgeOrder: AcknowledgeOrder =
            fun createAcknowledgementLetter sendAcknowledgement pricedOrder ->
                let letter = createAcknowledgementLetter pricedOrder
                let acknowledgement = {
                    EmailAddress = pricedOrder.CustomerInfo.EmailAddress
                    Letter = letter
                }
                
                match sendAcknowledgement acknowledgement with
                | Sent ->
                    let event = {
                        OrderId = pricedOrder.OrderId
                        EmailAddress = pricedOrder.CustomerInfo.EmailAddress 
                    }
                    Some event
                | NotSent ->
                    None
    
    module EventsModule =
        open WorkflowTypes
        open AcknowledgementModuel
        
        type OrderPlaced = PricedOrder
        type BillableOrderPlaced = {
            OrderId: OrderId
            BillingAddress: Address
            AmountToBill: BillingAmount
        }
        
        type PlaceOrderEvent =
            | OrderPlaced of OrderPlaced
            | BillableOrderPlaced of BillableOrderPlaced
            | AcknowledgementSent of OrderAcknowledgementSent
            
        type CreateEvents =
            PricedOrder
                -> OrderAcknowledgementSent option
                -> PlaceOrderEvent list
        
        let createBillingEvent (placedOrder: PricedOrder): BillableOrderPlaced option =
            let billingAmount = placedOrder.AmountToBill |> BillingAmount.value
            if billingAmount > 0.0m then
                let order = {
                    OrderId = placedOrder.OrderId
                    BillingAddress = placedOrder.BillingAddress
                    AmountToBill = placedOrder.AmountToBill
                }
                Some order
            else
                None
                
        let listOfOption opt =
            match opt with
            | Some x -> [x]
            | None -> []
        
        let createOrderPlacedEvent (placedOrder:PricedOrder) = placedOrder
        
        let createEvents: CreateEvents =
            fun pricedOrder acknowledgementEventOpt ->
                let event1 =
                    pricedOrder
                    |> createOrderPlacedEvent
                    |> PlaceOrderEvent.OrderPlaced
                    |> List.singleton
                let event2 =
                    acknowledgementEventOpt
                    |> Option.map PlaceOrderEvent.AcknowledgementSent
                    |> listOfOption
                let event3 =
                    pricedOrder
                    |> createBillingEvent
                    |> Option.map PlaceOrderEvent.BillableOrderPlaced
                    |> listOfOption
                
                [
                    yield! event1
                    yield! event2
                    yield! event3
                ]
    
    module WorkflowPipeline =
        open WorkflowTypes
        open ValidateOrderModule
        open PriceOrderModule
        open AcknowledgementModuel
        open EventsModule
        type Command<'data> = {
            Data: 'data
            Timestamp: System.DateTime
            UserId: string
        }

        type PlaceOrderCommand = Command<UnvalidatedOrder>
        // type AsyncResult<'success, 'failure> = Async<Result<'success, 'failure>>
        // type ValidationError = {
        //     FieldName: string
        //     ErrorDescription: string
        // }
        // type PlaceOrderError = ValidationError of ValidationError list
        // type PlaceOrderWorkflow = PlaceOrderCommand -> AsyncResult<PlaceOrderEvent list, PlaceOrderError>
        type PlaceOrderWorkflow = PlaceOrderCommand -> PlaceOrderEvent list
        
        
        
        
        
        
        
        
        
        let placeOrder: PlaceOrderWorkflow =
            fun unvalidatedOrder ->
                let validateOrder' =
                    unvalidatedOrder
                    |> validateOrder checkProductCodeExists checkAddressExists
                    
                let pricedOrder =
                    validatedOrder
                    |> priceOrder getProductPrice
                
                let acknowledgementOption =
                    pricedOrder
                    |> acknowledgeOrder createAcknowledgementLetter sendAcknowledgement
                let events =
                    createEvents pricedOrder acknowledgementOption
                events
                