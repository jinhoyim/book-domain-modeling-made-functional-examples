module ModelingWorkflowsAsPipelines.Sections.ShoppingCardStateMachineModule

type Item = { ProductCode: string; Quantity: int }
type ActiveCartData = { UnpaidItems: Item list }

type PaidCartData =
    { PaidItems: Item list; Payment: float }

type ShoppingCart =
    | EmptyCart
    | ActiveCart of ActiveCartData
    | PaidCart of PaidCartData

let addItem cart item =
    match cart with
    | EmptyCart ->
        // 새로운 장바구니를 만들고 아이템을 추가
        ActiveCart { UnpaidItems = [ item ] }
    | ActiveCart { UnpaidItems = existingItems } ->
        // 새로운 장바구니를 만들고 아이템을 추가
        ActiveCart { UnpaidItems = item :: existingItems }
    | PaidCart _ -> cart

let makePayment cart payment =
    match cart with
    | EmptyCart -> cart
    | ActiveCart { UnpaidItems = items } -> PaidCart { PaidItems = items; Payment = payment }
    | PaidCart _ -> cart