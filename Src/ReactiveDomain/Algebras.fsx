module ReactiveDomain.Algebras

#load "Aggregate.fsx"

open Aggregate

/// reactive domain algebra
module Domain =

    type Domain<'state, 'command, 'event, 'next> =
    | Handle of ('state * 'command) * ('event list Versioned -> 'next)
    | Rebuild of ('event list) * ('state -> 'next)

    let map f (d:Domain<'state, 'command, 'event, 'next>) = 
        match d with
        | Handle (arg, continuation) -> Handle (arg, continuation >> f)
        | Rebuild (arg, continuation) -> Rebuild (arg, continuation >> f)


module EventStore =
    
    type EventStore<'id, 'event, 'next> =
    | GetStream of 'id * ('event list -> 'next)
    | Append of ('id * 'event list Versioned) * (unit -> 'next)

    let map f (es:EventStore<'id, 'event, 'next>) =
        match es with
        | GetStream (aId, continuation) -> GetStream (aId, continuation >> f)
        | Append (aId, continuation) -> Append (aId, continuation >> f)

 
module EventBus =
    
    type EventBus<'id, 'event, 'next> =
    | Publish of ('id * 'event list) * (unit -> 'next)

    let map f = 
        function Publish (args, continuation) -> Publish (args, continuation >> f)


module Service =

    type Service<'id, 'command, 'event, 'state, 'next> =
    | Domain of Domain.Domain<'state, 'command, 'event, 'next>
    | EventStore of EventStore.EventStore<'id, 'event, 'next>
    | EventBus of EventBus.EventBus<'id, 'event, 'next>

    let map f (s:Service<'id, 'command, 'event, 'state, 'next>) =
        match s with
        | Domain d -> Domain.map f d |> Domain
        | EventStore es -> EventStore.map f es |> EventStore
        | EventBus eb -> EventBus.map f eb |> EventBus



module ServiceFree =
    
    open Service

    type Free<'id, 'command, 'event, 'state, 'a> =
    | Pure of 'a
    | Free of Service<'id, 'command, 'event, 'state, Free<'id, 'command, 'event, 'state, 'a>>

    [<RequireQualifiedAccess>]
    module Free =

        let lift svc = Free (map Pure svc)
        
        let rec bind f = function
            | Pure v -> f v
            | Free svc -> map (bind f) svc |> Free


    type Builder() =
        member __.Bind(svc, f) = Free.bind f svc
        member __.Return(v) = Pure v
        member __.Zero() = Pure ()

    let free = Builder()

