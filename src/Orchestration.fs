module OrchestrationCE.Orchestration
open OrchestrationCE.Workflow

type CircuitBreaker<'T1, 'T2> =
    | Continue of 'T1
    | Break of 'T2 list
    
type Orchestration<'a, 'b, 'c> = 'a -> Workflow<'a, CircuitBreaker<'b, 'c>>

let retn result _ = { Result = result |> Continue |> List.singleton; Next = None }

let map f orchestration =
    orchestration
    |> Workflow.map
        (function
            | Continue y -> Continue (f y)
            | Break y -> Break y)
        
let rec raiseToOrchestration workflow = function
    | Some event ->
        let { Result = result; Next = next } = workflow event
        { Result = result |> List.map Continue
          Next = Option.map raiseToOrchestration next }
    | None ->
        { Result = []; Next = Some (raiseToOrchestration workflow) }
        
let rec raiseToOrchestrationWithActions workflow actions = function
    | Some event ->
        let { Result = result; Next = next } = workflow event
        { Result = result |> List.map Continue
          Next = Option.map (fun orc -> raiseToOrchestrationWithActions orc actions) next }
    | None ->
        { Result = [(Break actions)]; Next = None }
        
let rec zip orchestration1 orchestration2 event =
    match (orchestration1 |> take 1) event, (orchestration2 |> take 1) event with
    | { Result = []; Next = Some next1 }, { Result = []; Next = Some next2 } ->
        { Result = []; Next = Some (zip next1 next2) }
    | { Result = [Continue result1]; Next = _ }, { Result = [Continue result2]; Next = _ } ->
        { Result = [Continue (result1, result2)]; Next = None }
    | { Result = [Continue result]; Next = _ }, { Result = []; Next = Some next } ->
        { Result = []; Next = Some (next |> map (fun x -> result, x)) }
    | { Result = []; Next = Some next }, { Result = [Continue result]; Next = _ } ->
        { Result = []; Next = Some (next |> map (fun x -> x, result)) }
    | { Result = [Break actions1]; Next = _ }, { Result = [Break actions2]; Next = _ } ->
        { Result = [(Break (actions1 @ actions2))]; Next = None }
    | { Result = [Break action]; Next = _ }, { Result = []; Next = _ } ->
        { Result = [(Break action)]; Next = None }
    | { Result = []; Next = _ }, { Result = [Break action]; Next = _ } ->
        { Result = [(Break action)]; Next = None }
    | { Result = []; Next = None }, _
    | _, { Result = []; Next = None } ->
        { Result = []; Next = None }
    | _, _ ->
        raise (exn "non singular results from take 1")
        
let private circuitBreak f = function
    | Continue y -> f y
    | Break z -> fun _ -> { Result = z |> Break |> List.singleton; Next = None }
        
type OrchestrationBuilder() =
    member __.Bind (m, f) =
        m |> take 1 |> switchMap (circuitBreak f)
        
    member __.Return(result) =
        retn result
        
    member _.MergeSources(orchestration1, orchestration2) = zip orchestration1 orchestration2
    
let orchestration = OrchestrationBuilder()