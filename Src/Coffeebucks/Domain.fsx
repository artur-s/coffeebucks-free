module Coffeebucks.Domain

#load "../ReactiveDomain\Aggregate.fsx"
    
open System
open ReactiveDomain.Aggregate

type Id = Id of Guid

type SKU = string
type TransactionId = Guid
type Money = decimal
type Reason = string
type Failure = string

type LineItem = {
    SKU:SKU
    UnitPrice:Money
    Quantity:decimal
    }

type Order = {
    Items:LineItem list
    }

type Payment = {
    Transaction:TransactionId
    Amount:Money
    }


type SalesOrder =
| Zero
| AwaitingPayment of Order * Payment list
| InPreparation of Order * Payment list * changeDue:Money option
| Ready of Order * Payment list * changeDue:Money option
| Cancelled of Order * Reason
// | Completed of Order * Payment list * changeDue:Money

type State = SalesOrder Versioned

type Command =
| PlaceOrder of Order
| AddMoreItems of LineItem list
| RemoveItems of SKU list
| AcceptPayment of Payment
| SetPrepared
| Cancel of Reason

type Event =
| OrderPlaced of Order
| MoreItemsAdded of LineItem list
| ItemsRemoved of SKU list
| PaymentAccepted of Payment
| Paid of changeDue:Money
| Prepared
| Cancelled of Reason


let handle (Versioned (currentOrder, version)) (command:Command) : Result<Event list Versioned, Failure> =

    let validateItems = function
        | [] -> Result.Error "No line items exist"
        | items when items |> List.distinctBy (fun i -> i.SKU, i.UnitPrice) <> items -> 
            Result.Error "SKU and UnitPrice must be unique within an order"
        | items -> Ok items
        
    let createOrder newOrder =
        newOrder.Items 
        |> validateItems
        |> Result.map (fun _ -> [Event.OrderPlaced newOrder])
        
    let addItems order items = 
        order.Items @ items
        |> validateItems
        |> Result.map (fun _ -> [MoreItemsAdded items])

    let removeItems payments skus =
        match payments, skus with
        | _::_, _ -> Error "Cannot remove items after payments have been accepted"
        | _, _::_ -> Ok [Event.ItemsRemoved skus]
        | _, [] -> Ok []

    let (|FullyPaid|NotYet|) (payments, order) =
        let paid = payments |> List.sumBy (fun p -> p.Amount)
        let total = order.Items |> List.sumBy (fun i -> i.Quantity)
        let remainder = total - paid

        if remainder > 0m then NotYet remainder else FullyPaid remainder
    
    let acceptPayment order paymentsSoFar newPayment =

        let payments = 
            paymentsSoFar @ [newPayment]
            |> List.distinct // idempotency on adding items
            
        let validPayment p = 
            if p.Amount <= 0m 
                then Result.Error "Payment amount must be greater than zero"
                else Ok p

        let validPayments ps =
            if ps |> List.distinctBy (fun p -> p.Transaction) = ps
                then Error <| sprintf "Payment transaction %A already taken" newPayment.Transaction
                else Ok ps

        validPayment newPayment
        |> Result.bind (fun _ -> validPayments payments)
        |> Result.map (fun _ -> 
            [
                yield Event.PaymentAccepted newPayment
                match payments, order with
                | FullyPaid changeDue -> yield Event.Paid changeDue
                | NotYet _ -> ()
            ])

    let setPrepared () = Ok [Event.Prepared]
    let cancelOrder reason = Ok [Event.Cancelled reason]

    let eventsRes =
        
        match currentOrder, command with

        | Zero, PlaceOrder newOrder -> createOrder newOrder
        | AwaitingPayment (order, _), AddMoreItems items -> addItems order items
        | AwaitingPayment (_, payments), RemoveItems skus -> removeItems payments skus
        | AwaitingPayment (order, paymentsSoFar), AcceptPayment newPayment -> acceptPayment order paymentsSoFar newPayment
        | AwaitingPayment (_), Cancel reason -> cancelOrder reason
        | InPreparation (_), SetPrepared -> setPrepared ()

        | Zero, _ -> Error "Order does not exist"
        | _, PlaceOrder _ -> Error "The order already exists"
        | _, AcceptPayment _ 
        | _, AddMoreItems _ 
        | _, RemoveItems _ -> Error "The order has already been paid"
        | _, SetPrepared -> Error "This order is not being prepared"
        | _, Cancel _ -> Error "Cannot cancel this order"


    eventsRes |> Result.map (fun evs -> Versioned (evs, version))


let apply event state : Result<SalesOrder, Failure> =

    match state, event with
    | Zero, Event.OrderPlaced newOrder -> 
        AwaitingPayment (newOrder, []) |> Ok
    | AwaitingPayment (order, payments), Event.MoreItemsAdded items -> 
        AwaitingPayment ({order with Items = order.Items @ items}, payments) |> Ok
    | AwaitingPayment (order, payments), Event.ItemsRemoved skus -> 
        let items = [for i in order.Items do
                        if skus |> List.contains i.SKU |> not then yield i]
        AwaitingPayment ({order with Items = items}, payments) |> Ok
    | AwaitingPayment (order, paymentsSoFar), Event.PaymentAccepted payment ->
        AwaitingPayment (order, paymentsSoFar @ [payment] |> List.distinct) |> Ok
    | AwaitingPayment (order, _), Event.Cancelled reason ->
        SalesOrder.Cancelled (order, reason) |> Ok
    | AwaitingPayment (order, payments), Event.Paid changeDue ->
        InPreparation (order, payments, Some changeDue) |> Ok
    | AwaitingPayment (order, payments), Event.Prepared -> Ready (order, payments, None) |> Ok
    | InPreparation (order, payments, change), Event.Prepared -> Ready (order, payments, change) |> Ok
    | s, e ->
        let ucName (uc:'DU) = Reflection.FSharpValue.GetUnionFields(uc, typeof<'DU>) |> function ci, _ -> ci.Name
        Error <| sprintf "Event application error. Cannot apply event '%s' to state '%s'" (ucName e) (ucName s)


let rebuild events : Result<State, Failure> =

    let folder state event =
        state |> Result.bind (fun (Versioned (s, v)) -> 
            apply event s 
            |> Result.map (fun ns -> ns, v+1)
            |> Result.map Versioned)
        

    events |> List.fold folder (Ok (Versioned.Create(Zero, -1)))
    