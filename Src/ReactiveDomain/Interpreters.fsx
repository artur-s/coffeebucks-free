module ReactiveDomain.Interpreters

#load "Algebras.fsx"

type Error = string
type IO<'output> = Async<Result<'output,Error>>

module IO =

    let bind (f:'a -> IO<'b>) (io:IO<'a>) : IO<'b> = 
        async {
            let! result = io
            return!
                match result with
                | Ok v -> f v
                | Result.Error err -> async { return Result.Error err }}

    let (>>=) io f = bind f io
    let retn v = async { return Ok v}
    let ofResult r : IO<_> = async { return r}

    type Builder() =
        member __.Bind(io, f) = bind f io
        member __.Return(v) = retn v

    /// Represents a side-effecting operation
    let io = Builder()



module Service =
    
    open IO
    open Algebras.ServiceFree
    open Algebras.Service

    let rec interpret domain eventStore eventBus (program:Free<_,_,_,_,_>) =
        let recurse = interpret domain eventStore eventBus
        match program with
        | Pure r -> IO.retn r
        | Free svc ->
            match svc with
            | Domain d -> domain d >>= recurse
            | EventStore es -> eventStore es >>= recurse
            | EventBus eb -> eventBus eb >>= recurse

