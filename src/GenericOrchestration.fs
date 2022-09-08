module OrchestrationCE.GenericOrchestration

open OrchestrationCE
open OrchestrationCE.Coordination

let retn innerRetn x =
    innerRetn x
    |> retn

let map innerMap f orchestration =
    innerMap f
    |> fun mapper -> map mapper orchestration
    
let switchMap innerCombine f = switchMap (innerCombine f)
let mergeMap innerCombine f = mergeMap (innerCombine f)
let concatMap innerCombine f = concatMap (innerCombine f)
let exhaustMap innerCombine f = exhaustMap (innerCombine f)
   
let rec private transformZipResults innerMerge { Result = results; Next = next } =
    { Result = List.map innerMerge results
      Next = Option.map (fun next -> next >> transformZipResults innerMerge) next }
    
let zip innerMerge orchestration1 orchestration2 =
    zip orchestration1 orchestration2
    >> transformZipResults innerMerge

