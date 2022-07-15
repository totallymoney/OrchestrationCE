module OrchestrationCETests.ExhaustMapTests

open Expecto
open OrchestrationCE
open Workflow
open TestEvents

[<Tests>]
let exhaustMapTests =
    testList "exhaustMap tests" [
        testCase "1" <| fun _ ->
            let testOrchestration =
                (eventReceived |> filter (fun x -> x.Id = 1)) 
                |> exhaustMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 1))
                
            let result = testOrchestration (createEventString 1)
            do Expect.contains result.Result { Id = 1 } ""
            do Expect.hasLength result.Result 1 ""
            do Expect.isSome result.Next ""
            
            let result2 = testOrchestration (createEventString 1)
                          |> (fun r -> r.Next.Value(createEventString 1))
                        
            do Expect.contains result2.Result { Id = 1 } ""
            do Expect.hasLength result2.Result 1 ""
            do Expect.isSome result2.Next ""
            
        testCase "2" <| fun _ ->
            let testOrchestration =
                (eventReceived |> filter (fun x -> x.Id = 1))
                |> exhaustMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 2))
                
            let result = testOrchestration (createEventString 1)
                         |> (fun r -> r.Next.Value(createEventString 2))
                         
            do Expect.contains result.Result { Id = 2 } ""
            do Expect.hasLength result.Result 1 ""
            do Expect.isSome result.Next ""
        
        testCase "3" <| fun _ ->
            let testOrchestration =
                eventReceived |> filter (fun x -> x.Id > 10)
                |> exhaustMap (fun x -> eventReceived |> filter (fun y -> y.Id = x.Id - 10) |> take 1)
                
            let result = testOrchestration (createEventString 11)
                         |> (fun r -> r.Next.Value(createEventString 12))
                         |> (fun r -> r.Next.Value(createEventString 2))
                         
            do Expect.isEmpty result.Result ""
            do Expect.isSome result.Next ""
            
            let r = testOrchestration (createEventString 11)
            let r2 = r.Next.Value (createEventString 12)
                    
            let r3 = r2.Next.Value (createEventString 1)
                    
            do Expect.isSome r3.Next ""
            
            let result2 = testOrchestration (createEventString 11)
                          |> (fun r -> r.Next.Value(createEventString 12))
                          |> (fun r -> r.Next.Value(createEventString 13))
                          |> (fun r -> r.Next.Value(createEventString 2))
                          |> (fun r -> r.Next.Value(createEventString 3))
                          |> (fun r -> r.Next.Value(createEventString 14))
                          |> (fun r -> r.Next.Value(createEventString 4))
                          |> (fun r -> r.Next.Value(createEventString 1))

            let list = result2.Result
            
            do Expect.equal list [{ Id = 1 }] ""
            do Expect.isSome result2.Next ""
            
            let result3 = testOrchestration (createEventString 11)
                          |> (fun r -> r.Next.Value(createEventString 12))
                          |> (fun r -> r.Next.Value(createEventString 13))
                          |> (fun r -> r.Next.Value(createEventString 2))
                          |> (fun r -> r.Next.Value(createEventString 3))                      
                          |> (fun r -> r.Next.Value(createEventString 14))
                          |> (fun r -> r.Next.Value(createEventString 4))
                          |> (fun r -> r.Next.Value(createEventString 1))
                          |> (fun r -> r.Next.Value(createEventString 15))
                          |> (fun r -> r.Next.Value(createEventString 5))
                          
            let list = result3.Result
            
            do Expect.equal list [{ Id = 5 }] ""
            do Expect.isSome result3.Next "" ]