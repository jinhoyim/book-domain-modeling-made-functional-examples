module EvolvingDesignKeepingItClean.AddingSupportVIPCustomers
//
// type CustomerInfo = {
//     IsVip: bool
// }
//
// type CustomerStatus =
//     | Normal of CustomerInfo
//     | Vip of CustomerInfo

// type Order = {
//     CustomerStatus: CustomerStatus
// }

type VipStatus =
    | Normal
    | Vip

type CustomerInfo = {
    VipStatus: VipStatus
}
