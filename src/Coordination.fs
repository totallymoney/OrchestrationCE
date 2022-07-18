module OrchestrationCE.Coordination

type Coordination<'TEvent, 'TData> = 'TEvent -> CoordinationResult<'TEvent, 'TData>
and CoordinationResult<'TEvent, 'TData> =
    { Result: 'TData list
      Next: Coordination<'TEvent, 'TData> option }
    
let retn result _ = { Result = List.singleton result; Next = None }

let empty = fun _ -> { Result = []; Next = None }

let rec map f coordination =
    coordination
    >> fun { Result = x; Next = next } ->
        { Result = List.map f x; Next = Option.map (map f) next }
        
let rec private switch' currentCoordination higherOrderCoordination event =
    match (higherOrderCoordination event), currentCoordination with
    | { Result = []; Next = None }, None ->
        { Result = []; Next = None }
    | { Result = []; Next = None }, Some currentCoordination ->
        currentCoordination event
    | { Result = []; Next = Some nextHigherOrderCoordination }, None ->
        { Result = []; Next = Some (switch' currentCoordination nextHigherOrderCoordination) }
    | { Result = []; Next = Some nextHigherOrderCoordination }, Some currentCoordination ->
        let { Result = result; Next = next } = currentCoordination event
        { Result = result; Next = Some (switch' next nextHigherOrderCoordination) }
    | { Result = coordinations; Next = None }, _ ->
        (List.last coordinations) event
    | { Result = workflows; Next = Some nextHigherOrderCoordination }, _ ->
        let { Result = result; Next = next } = (List.last workflows) event
        { Result = result; Next = Some (switch' next nextHigherOrderCoordination) }
        
let rec private flatten flattenPair higherOrderCoordination event =
    higherOrderCoordination event
    |> (function
        | { Result = coordinations; Next = None } ->
          List.map (fun c -> c event) coordinations 
          |> List.fold (fun acc res -> flattenPair (acc, res)) { Result = []; Next = None }
        | { Result = workflows; Next = Some nextHigherOrderCoordination } ->
          List.map (fun c -> c event) workflows
          |> List.fold (fun acc res -> flattenPair (acc, res)) { Result = []; Next = None }
          |> (function
            | { Result = result; Next = Some next } ->
                { Result = result
                  Next = Some (fun e -> flattenPair (next e, (flatten flattenPair nextHigherOrderCoordination e))) } 
            | { Result = result; Next = None } ->
                { Result = result
                  Next = Some (flatten flattenPair nextHigherOrderCoordination) }))
    
let rec private mergePair = function
    | { Result = result1; Next = Some nextFn1 }, { Result = result2; Next = Some nextFn2 } ->
            { Result = List.append result1 result2
              Next = Some (fun event -> mergePair (nextFn1 event, nextFn2 event)) } 
    | { Result = result1; Next = Some nextFn }, { Result = result2;  Next = None }
    | { Result = result1;  Next = None }, { Result = result2;  Next = Some nextFn } ->
        { Result = List.append result1 result2; Next = Some nextFn }
    | { Result = result1; Next = None }, { Result = result2; Next = None } ->
      { Result = List.append result1 result2; Next = None }         

let rec private concatPair = function
    | { Result = result1; Next = None }, { Result = result2;  Next = next } ->
        { Result = List.append result1 result2; Next = next }
    | { Result = result; Next = Some next }, ({ Next = None; } as wfResult2) ->
        { Result = result; Next = Some (fun e -> concatPair (next e, wfResult2)) }
    | { Result = result1; Next = Some next1 }, { Result = result2; Next = Some next2 } ->
        let next2'' e =
            let { Result = result''; Next = next2' } = next2 e
            { Result = List.append result2 result''; Next = next2' }
        { Result = result1; Next = Some (fun e -> concatPair ((next1 e), next2'' e)) }
            
let rec private exhaustPair = function
    | { Result = result1; Next = None }, { Result = result2; Next = next2 }->
        { Result = List.append result1 result2; Next = next2 }
    | { Result = result; Next = Some next }, { Next = None; } ->
        { Result = result; Next = Some next }
    | { Result = result; Next = Some next1 }, { Next = Some next2; } ->
        { Result = result; Next = Some (fun e -> exhaustPair (next1 e, next2 e)) }
        
let merge orc' = flatten mergePair orc'
let switch coordination' = switch' None coordination'
let concat orc' = flatten concatPair orc'
let exhaust orc' = flatten exhaustPair orc'
let mergeMap f = map f >> merge
let switchMap f = map f >> switch
let concatMap f = map f >> concat
let exhaustMap f = map f >> exhaust

let rec filter predicate coordination =
    coordination
    >> (fun { Result = result; Next = next } ->
        let results' = List.filter predicate result
        { Result = results'; Next = Option.map (filter predicate) next })
    
let rec choose chooser coordination =
    coordination
    >> (fun { Result = result; Next = next } ->
        let results' = List.choose chooser result
        { Result = results'; Next = Option.map (choose chooser) next })
    
let rec take count coordination =
    coordination
    >> fun { Result = result;  Next = next } ->
            let results' = result |> List.truncate count
            let remainingCount = count - results'.Length
            let next' = if remainingCount > 0
                        then Option.map (take remainingCount) next
                        else None
            { Result = results';  Next = next' }

let rec zip coordination1 coordination2 e =
    match (coordination1 |> take 1) e, (coordination2 |> take 1) e with
    | { Result = []; Next = Some next1 }, { Result = []; Next = Some next2 } ->
        { Result = []; Next = Some (zip next1 next2) }
    | { Result = [result1]; Next = _ }, { Result = [result2]; Next = _ } ->
        { Result = [(result1, result2)]; Next = None }
    | { Result = [result]; Next = _ }, { Result = []; Next = Some next } ->
        { Result = []; Next = Some (next |> map (fun x -> result, x)) }
    | { Result = []; Next = Some next }, { Result = [result]; Next = _ } ->
        { Result = []; Next = Some (next |> map (fun x -> x, result)) }
    | { Result = []; Next = None }, _
    | _, { Result = []; Next = None } ->
        { Result = []; Next = None }
    | _, _ ->
        raise (exn "non singular results from take 1")
        
let (<&>) coordination coordination2 event =
    concatPair ((coordination event), (coordination2 event))
        
type CoordinationBuilder() =
    member __.Bind (m, f) =
        m |> take 1 |> switchMap f
        
    member __.Return(m) =
        retn m
        
    member _.MergeSources(coordination1, coordination2) = zip coordination1 coordination2
     
let coordination = CoordinationBuilder()