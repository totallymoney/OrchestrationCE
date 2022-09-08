module OrchestrationCE.CancellableOrchestration

open OrchestrationCE.Coordination
open OrchestrationCE.GenericOrchestration

type CancellableCircuitBreaker<'T1, 'T2, 'T3> =
    | Continue of 'T1
    | Cancel of 'T2
    | Break of 'T3 list
    
type CancellableOrchestrationArgument<'TEvent> =
    | Event of 'TEvent
    | NoMoreEvents
    | DoCancel
    
module CancellableCircuitBreaker =
    let map f = function
        | Continue y -> Continue (f y)
        | Cancel y -> Cancel y
        | Break y -> Break y
        
    let retn x =
        Continue x
        
    let combine cancelf breakf continuef = function
        | Continue x -> continuef x
        | Cancel x -> cancelf x
        | Break x -> breakf x
        
    let merge combineCancel = function
        | Cancel x, Cancel y -> Cancel (combineCancel x y)
        | Cancel x, _
        | _, Cancel x -> Cancel x
        | Continue x, Continue y -> Continue (x, y)
        | Break x, Continue _
        | Continue _, Break x -> Break x
        | Break x, Break y -> Break (x @ y)
        
type CancellableOrchestration<'a, 'b, 'c, 'd> =
    Coordination<CancellableOrchestrationArgument<'a>, CancellableCircuitBreaker<'b, 'c, 'd>>

let switchMap f = switchMap (CancellableCircuitBreaker.combine (Cancel >> Coordination.retn) (Break >> Coordination.retn)) f
let mergeMap f = mergeMap (CancellableCircuitBreaker.combine (Cancel >> Coordination.retn) (Break >> Coordination.retn)) f
let concatMap f = concatMap (CancellableCircuitBreaker.combine (Cancel >> Coordination.retn) (Break >> Coordination.retn)) f
let exhaustMap f = exhaustMap (CancellableCircuitBreaker.combine (Cancel >> Coordination.retn) (Break >> Coordination.retn)) f

let rec raiseToCancellableOrchestration returnOnCancel workflow = function
    | Event event ->
        let { Result = result; Next = next } = workflow event
        { Result = result |> List.map Continue
          Next = Option.map (raiseToCancellableOrchestration returnOnCancel) next }
    | NoMoreEvents ->
        { Result = []; Next = None }
    | DoCancel ->
        { Result = [(Cancel returnOnCancel)]
          Next =  None }
        
let rec raiseToCancellableOrchestrationWithActions returnOnCancel actions workflow = function
    | Event event ->
        let { Result = result; Next = next } = workflow event
        { Result = result |> List.map Continue
          Next = Option.map (raiseToCancellableOrchestrationWithActions returnOnCancel actions) next }
    | NoMoreEvents ->
        { Result = [(Break actions)]; Next = None }
    | DoCancel ->
        { Result = [(Cancel returnOnCancel)]
          Next =  None }
        
type CancellableOrchestrationBuilder<'TCancellationResponse>(combineCancel: 'TCancellationResponse -> 'TCancellationResponse -> 'TCancellationResponse) =
    member _.Bind (m, f) =
        m
        |> take 1
        |> switchMap f
   
    member _.Return(result) =
        retn CancellableCircuitBreaker.retn result
        
    member _.ReturnFrom (m) =
        m
        
    member _.MergeSources(orchestration1, orchestration2) =
        zip (CancellableCircuitBreaker.merge combineCancel) (orchestration1 |> take 1) (orchestration2 |> take 1)
    
    member _.Zero() =
        retn CancellableCircuitBreaker.retn ()
    
let cancellableOrchestration combineCancel = CancellableOrchestrationBuilder(combineCancel)

let rec private switchWhileKeepingContext' currentCoordination currentContext higherOrderCoordination event =
    match (higherOrderCoordination event), currentCoordination with
    | { Result = []; Next = None }, None ->
        { Result = []; Next = None }
    | { Result = []; Next = None }, Some currentCoordination ->
        let { Result = result; Next = next } = currentCoordination event
        { Result = List.map (fun x -> x, currentContext) result
          Next = Option.map (Coordination.map (fun x -> x, currentContext)) next }
    | { Result = []; Next = Some nextHigherOrderCoordination }, None ->
        { Result = []; Next = Some (switchWhileKeepingContext' currentCoordination currentContext nextHigherOrderCoordination) }
    | { Result = []; Next = Some nextHigherOrderCoordination }, Some currentCoordination ->
        let { Result = result; Next = next } = currentCoordination event
        { Result = List.map (fun x -> x, currentContext) result
          Next = Some (switchWhileKeepingContext' next currentContext nextHigherOrderCoordination) }
    | { Result = coordinations; Next = None }, None ->
       let { Result = result; Next = next } = (List.last coordinations) event
       { Result = List.map (fun x -> x, currentContext) result
         Next = Option.map (Coordination.map (fun x -> x, currentContext)) next }
    | { Result = coordinations; Next = None }, Some currentCoordination ->
       let { Result = cancelResults } = currentCoordination DoCancel
       let newContext = cancelResults |> List.tryHead |> Option.defaultValue currentContext
       let { Result = result; Next = next } = (List.last coordinations) event
       { Result = List.map (fun x -> x, currentContext) result
         Next = Option.map (Coordination.map (fun x -> x, newContext)) next }
    | { Result = coordinations; Next = Some nextHigherOrderCoordination }, None ->
        let { Result = result; Next = next } = (List.last coordinations) event
        { Result = List.map (fun x -> x, currentContext) result
          Next = Some (switchWhileKeepingContext' next currentContext nextHigherOrderCoordination) }
    | { Result = coordinations; Next = Some nextHigherOrderCoordination }, Some currentCoordination ->
        let { Result = cancelResults } = currentCoordination DoCancel
        let newContext = cancelResults |> List.tryHead |> Option.defaultValue currentContext
        let { Result = result; Next = next } = (List.last coordinations) event
        { Result = List.map (fun x -> x, currentContext) result
          Next = Some (switchWhileKeepingContext' next newContext nextHigherOrderCoordination) }
        
let switchWhileKeepingContext currentContext higherOrderCoordination event =
    switchWhileKeepingContext' None currentContext higherOrderCoordination event