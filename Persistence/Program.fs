// // workflow mixes domain logic and I/O
// let payInvoice invoiceId payment =
//     // load from db
//     let invoice = loadInvoiceFromDatabase(invoiceId)
//     
//     // apply payment
//     invoice.ApplyPayment(payment)
//     
//     // handle different outcomes
//     if invoice.IsFullyPaid then
//         markAsFullyPaidInDb(invoiceId)
//         postInvoicePaidEvent(invoiceId)
//     else
//         markAsPartiallyPaidInDb(invoiceId)

type InvoicePaymentResult =
    | FullyPaid
    | PartiallyPaid
    
type PayInvoiceCommand = {
    InvoiceId: string
    Payment: decimal
}

let applyPayment unpaidInvoice payment: InvoicePaymentResult =
    let updatedInvoice = unpaidInvoice |> applyPayment payment

    if isFullyPaid updatedInvoice then
        FullyPaid
    else
        PartiallyPaid updatedInvoice

let payInvoice
    loadUnpaidInvoiceFromDatabase   // dependency
    markAsFullyPaidInDb             // dependency
    updateInvoiceInDb               // dependency
    payInvoiceCommand =
        
    // load from db
    let invoiceId = payInvoiceCommand.InvoiceId
    let unpaidInvoice = loadUnpaidInvoiceFromDatabase invoiceId // I/O
    
    // call into pure domain
    let payment = payInvoiceCommand.Payment
    let paymentResult = applyPayment unpaidInvoice payment // pure
    
    // handle result
    match paymentResult with
    | FullyPaid ->
        markAsFullyPaidInDb invoiceId // I/O
        postInvoicePaidEvent invoiceId // I/O
    | PartiallyPaid updatedInvoice ->
        updateInvoiceInDb updatedInvoice // I/O
    