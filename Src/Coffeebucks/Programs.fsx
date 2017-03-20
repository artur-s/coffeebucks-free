module Coffeebucks.Programs

#load "../ReactiveDomain/Algebras.fsx"

open ReactiveDomain.Algebras
open ReactiveDomain.Algebras.ServiceFree

module private Domain =
    
    open ReactiveDomain.Algebras.Domain

    let handle v : Free<'id, 'command, 'event, 'state, _> = 
        Free.lift(Handle(v, id) |> Service.Domain)

    let rebuid v : Free<'id, 'command, 'event, 'state, _> = 
        Free.lift(Rebuild(v, id) |> Service.Domain)


module private EventStore =

    open ReactiveDomain.Algebras.EventStore

    let getStream v : Free<'id, 'command, 'event, 'state, _> = 
        Free.lift(GetStream(v,id) |> Service.EventStore)
    let append v : Free<'id, 'command, 'event, 'state, _> = 
        Free.lift(Append(v,id) |> Service.EventStore)


module private EventBus =

    open ReactiveDomain.Algebras.EventBus

    let publish v = Free.lift(Publish(v, id) |> Service.EventBus)


module Commands =

    open ReactiveDomain.Aggregate

    let handle (aId:'id, command:'command) =
        free {
            let! events = EventStore.getStream aId
            let! (state:'state) = Domain.rebuid events
            let! newEvents = Domain.handle (state, command)
            do! EventStore.append (aId, newEvents)
            let (Versioned (newEvents:'event list,_)) = newEvents
            do! EventBus.publish (aId, newEvents)
        }