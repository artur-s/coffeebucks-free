module Coffeebucks.Interpreters

#load "../ReactiveDomain\Interpreters.fsx"
#load "Domain.fsx"

open ReactiveDomain
open Aggregate
open Interpreters

module Domain =

    open Algebras.Domain

    let interpret program : IO<'output> = 
        match program with
        | Handle ((state, command), continuation) -> Domain.handle state command 
                                                     |> Result.map continuation
                                                     |> IO.ofResult
        | Rebuild (events, continuation) -> Domain.rebuild events
                                            |> Result.map continuation
                                            |> IO.ofResult
    

module InMemoryEventStore =
    
    open System.Collections.Concurrent
    open Algebras.EventStore
    open Coffeebucks.Domain
    open IO

    type StreamId = string
    type Stream = Domain.Event Versioned list

    let store = ConcurrentDictionary<StreamId, Stream>()

    let append (streamId, events, expected:Version) : Async<Result<Unit,Error>> =
        async {
            let versionedEvents = 
                events |> List.mapi (fun i ev -> (Versioned(ev, expected + 1 + i)))
            
            let mutable added = false

            let add _ =
                added <- true
                versionedEvents
            
            let update _ stream = 
                if (stream |> List.last |> (function Versioned (_, ver) -> ver)) = expected
                    then stream @ versionedEvents
                    else stream

            let addedOrUpdated = store.AddOrUpdate(streamId, add, update)
            
            return
                if addedOrUpdated = versionedEvents && not added
                    then Result.Error "Concurrency error. Retry operation"
                    else Ok ()
        }

    let tryGet streamId : Async<Stream option> =
        async {
            return
                match store.TryGetValue(streamId) with
                | true, stream -> Some stream
                | _ -> None
        }


    let interpret (program:EventStore<Id, Domain.Event,_>) : IO<'output> =

        match program with

        | GetStream (Id aggrId, continuation) -> 
            async {
                let! streamOpt = tryGet (string aggrId)
                return
                    match streamOpt with
                    | Some stream ->
                        stream |> List.map (function (Versioned (ev,_)) -> ev)
                    | None -> []
                    |> continuation |> Ok
            }

        | Append ((Id aggrId, Versioned (newEvents, expectedVersion)), continuation) -> 
            append (string aggrId, newEvents, expectedVersion)
            >>= (continuation >> IO.retn)



module EventBus =
    
    open System.Collections.Concurrent
    open Algebras.EventBus
    open Coffeebucks.Domain

    type HandleEvent = Id * Domain.Event -> Async<unit>

    let private handlers = ConcurrentBag<HandleEvent>()

    let subscribe handler =
        handlers.Add handler

    let publish (aggrId, events) = async {
        for handle in handlers do
            for event in events do
                do! handle(aggrId, event)
    }        

    let interpret program : IO<'output> = async {
        let (Publish ((aggrId, events:Domain.Event list), continuation)) = program
        do! publish (aggrId, events)
        return continuation () |> Ok
        }


// try "GetEventStore" interpreter
// try "GitStore" event store interpreter

