module OrchestrationCETests.OperationsTests

open Expecto
open OrchestrationCE.Coordination

type Event =
    | Event1 of bool
    
let (|>>) orcResult =
    orcResult.Next.Value 
    
let complete orc =
    orc.Next.Value None
    
let result orc =
    orc.Result

[<Tests>]
let operationsTests =
    testList "operations" [
        testCase "distinctUntilChanged" <| fun _ ->
            let coordination =
                event (function | Event1 x -> Some x)
                |> distinctUntilChanged
                
            let { Result = result; Next = next } = coordination (Event1 false)
            Expect.equal result [ false ] ""
                
            let { Result = result2; Next = next2 } = next.Value (Event1 false)
            Expect.isEmpty result2 ""
            
            let { Result = result3; Next = next3 } = next2.Value (Event1 false)
            Expect.isEmpty result3 ""
            
            let { Result = result4; Next = next4 } = next3.Value (Event1 true)
            Expect.equal result4 [ true ] ""
            
            let { Result = result5 } = next4.Value (Event1 true)
            Expect.isEmpty result5 ""
        
        testCase "takeUntil" <| fun _ ->
            let coordination =
                event (function | Event1 x -> Some x)
                |> takeWhile not
                
            let { Result = result; Next = next } = coordination (Event1 false)
            Expect.equal result [ false ] ""
                
            let { Result = result2; Next = next2 } = next.Value (Event1 false)
            Expect.equal result2 [ false ] ""
            
            let { Result = result3; Next = next3 } = next2.Value (Event1 false)
            Expect.equal result3 [ false ] ""
            
            let { Result = result4; Next = next4 } = next3.Value (Event1 true)
            Expect.isEmpty result4 ""
            Expect.isNone next4 ""
            
        ]
    
