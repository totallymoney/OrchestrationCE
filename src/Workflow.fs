module OrchestrationCE.Workflow

type Workflow<'TEvent, 'TData> = 'TEvent -> WorkflowResult<'TEvent, 'TData>
and WorkflowResult<'TEvent, 'TData> =
    { Result: 'TData list
      Next: Workflow<'TEvent, 'TData> option }
    
let retn result _ = { Result = List.singleton result; Next = None }

let empty = fun _ -> { Result = []; Next = None }

let rec map f workflow =
    workflow
    >> fun { Result = x; Next = next } ->
        { Result = List.map f x; Next = Option.map (map f) next }
        
let rec private switch' currentWF higherOrderWF event =
    match (higherOrderWF event), currentWF with
    | { Result = []; Next = None }, None ->
        { Result = []; Next = None }
    | { Result = []; Next = None }, Some currentWF ->
        currentWF event
    | { Result = []; Next = Some nextHigherOrderWF }, None ->
        { Result = []; Next = Some (switch' currentWF nextHigherOrderWF) }
    | { Result = []; Next = Some nextHigherOrderWF }, Some currentWF ->
        let { Result = result; Next = next } = currentWF event
        { Result = result; Next = Some (switch' next nextHigherOrderWF) }
    | { Result = workflows; Next = None }, _ ->
        (List.last workflows) event
    | { Result = workflows; Next = Some nextHigherOrderWorkflow }, _ ->
        let { Result = result; Next = next } = (List.last workflows) event
        { Result = result; Next = Some (switch' next nextHigherOrderWorkflow) }
        
let rec private flatten flattenPair higherOrderWorkflow event =
    higherOrderWorkflow event
    |> (function
        | { Result = workflows; Next = None } ->
          List.map (fun wf -> wf event) workflows 
          |> List.fold (fun acc res -> flattenPair (acc, res)) { Result = []; Next = None }
        | { Result = workflows; Next = Some nextHigherOrderWF } ->
          List.map (fun wf -> wf event) workflows
          |> List.fold (fun acc res -> flattenPair (acc, res)) { Result = []; Next = None }
          |> (function
            | { Result = result; Next = Some next } ->
                { Result = result
                  Next = Some (fun e -> flattenPair (next e, (flatten flattenPair nextHigherOrderWF e))) } 
            | { Result = result; Next = None } ->
                { Result = result
                  Next = Some (flatten flattenPair nextHigherOrderWF) }))
    
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
let switch orc' = switch' None orc'
let concat orc' = flatten concatPair orc'
let exhaust orc' = flatten exhaustPair orc'
let mergeMap f = map f >> merge
let switchMap f = map f >> switch
let concatMap f = map f >> concat
let exhaustMap f = map f >> exhaust

let rec filter predicate orc =
    orc
    >> (fun { Result = result; Next = next } ->
        let results' = List.filter predicate result
        { Result = results'; Next = Option.map (filter predicate) next })
    
let rec take count orc =
    orc
    >> fun { Result = result;  Next = next } ->
            let results' = result |> List.truncate count
            let remainingCount = count - results'.Length
            let next' = if remainingCount > 0
                        then Option.map (take remainingCount) next
                        else None
            { Result = results';  Next = next' }

let rec zip workflow1 workflow2 e =
    match (workflow1 |> take 1) e, (workflow2 |> take 1) e with
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
        
type WorkflowBuilder() =
    member __.Bind (m, f) =
        m |> take 1 |> switchMap f
        
    member __.Return(m) =
        retn m
        
    member _.MergeSources(orc1, orc2) = zip orc1 orc2
     
let workflow = WorkflowBuilder()