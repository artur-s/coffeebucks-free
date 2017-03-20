module Coffeebucks.Api

#load "Programs.fsx"
#load "Interpreters.fsx"

open ReactiveDomain.Interpreters
open Coffeebucks.Domain
open Coffeebucks.Programs
open Coffeebucks.Interpreters


let private interpret program =
    Service.interpret
        Domain.interpret
        InMemoryEventStore.interpret
        EventBus.interpret
        program
        
let execute (aggrId:Id, command) = 
    Commands.handle(aggrId, command) 
    |> interpret

let placeOrder (newId:System.Guid, order) =
    execute (Id newId, Domain.Command.PlaceOrder(order))

let addMoreItems (aggrId, items) =
    execute (Id aggrId, Domain.Command.AddMoreItems items)

let removeItems (aggrId, items) =
    execute (Id aggrId, Domain.Command.RemoveItems items)

let acceptPayment (aggrId, payment) =
    execute (Id aggrId, Domain.Command.AcceptPayment payment)

let setPrepared aggrId =
    execute (Id aggrId, Domain.Command.SetPrepared)

let cancel (aggrId, reason) = 
    execute (Id aggrId, Domain.Command.Cancel reason)




