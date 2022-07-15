module OrchestrationCETests.MergeMapTests


open Expecto
open OrchestrationCE
open Workflow
open TestEvents

[<Tests>]
let mergeMapTests =
    testList "mergeMap tests" [
        testCase "1" <| fun _ ->
            let testOrchestration =
                (eventReceived |> filter (fun x -> x.Id = 1))
                |> mergeMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 1))
                
            let event = createEventString 1
            let result = testOrchestration event
            do Expect.contains result.Result { Id = 1 } ""
            do Expect.hasLength result.Result 1 ""
            do Expect.isSome result.Next ""
            
            let result2 = result.Next.Value event
            let list = result2.Result |> Seq.toList
            do Expect.equal list [{ Id = 1 }; { Id = 1 }] ""
            do Expect.isSome result2.Next ""
            
            let result3 = result2.Next.Value event
            let list = result3.Result |> Seq.toList
            do Expect.equal list [{ Id = 1 }; { Id = 1 }; { Id = 1 }] ""
            do Expect.isSome result3.Next ""
            
        testCase "2" <| fun _ ->
            let testOrchestration =
                (eventReceived |> filter (fun x -> x.Id = 1) |> take 1)
                |> mergeMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 1) |> take 1)
                
            let event = createEventString 1
            let result = testOrchestration event
            do Expect.contains result.Result { Id = 1 } ""
            do Expect.hasLength result.Result 1 ""
            do Expect.isNone result.Next ""
            
        testCase "3" <| fun _ ->
            let testOrchestration =
                eventReceived |> filter (fun x -> x.Id = 1)
                |> mergeMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 1) |> take 1)
                
            let event = createEventString 1
            let result = testOrchestration event
            do Expect.contains result.Result { Id = 1 } ""
            do Expect.hasLength result.Result 1 ""
            do Expect.isSome result.Next ""
            
            let result2 = result.Next.Value event
            do Expect.contains result2.Result { Id = 1 } ""
            do Expect.hasLength result2.Result 1 ""
            do Expect.isSome result2.Next ""
            
        testCase "4" <| fun _ ->
            let testOrchestration =
                (eventReceived |> filter (fun x -> x.Id = 1) |> take 1)
                |> mergeMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 1))
                
            let event = createEventString 1
            let result = testOrchestration event
            do Expect.contains result.Result { Id = 1 } ""
            do Expect.hasLength result.Result 1 ""
            do Expect.isSome result.Next ""
            
            let result2 = result.Next.Value event
            do Expect.contains result2.Result { Id = 1 } ""
            do Expect.hasLength result2.Result 1 ""
            do Expect.isSome result2.Next ""
            
        testCase "5" <| fun _ ->
            let testOrchestration =
                (eventReceived |> filter (fun x -> x.Id = 1))
                |> mergeMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 2))
            
            let result = testOrchestration (createEventString 2)
                         
            do Expect.isEmpty result.Result ""
            do Expect.isSome result.Next ""
            
            let result2 = result.Next.Value (createEventString 1)
                          
            do Expect.isEmpty result2.Result ""
            do Expect.isSome result2.Next ""
            
            let result3 = result2.Next.Value (createEventString 2)
            let list = result3.Result
            do Expect.equal list [{ Id = 2 }] ""
            
            let result4 = result3.Next.Value (createEventString 2)
            let list = result4.Result
            do Expect.equal list [{ Id = 2 }] ""
            
            let result5 = result4.Next.Value (createEventString 1)
            do Expect.isEmpty result5.Result ""
            do Expect.isSome result5.Next ""
            
            let result6 = result5.Next.Value (createEventString 2)
            let list = result6.Result
            do Expect.equal list [{ Id = 2 }; { Id = 2 }] ""
            
        testCase "6" <| fun _ ->
            let testOrchestration =
                (eventReceived |> filter (fun x -> x.Id = 1))
                |> mergeMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 2))
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 3) |> take 1)
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 4) |> take 1)
                
            let result = testOrchestration (createEventString 1)
                         |> (fun r -> r.Next.Value(createEventString 2))
                         |> (fun r -> r.Next.Value(createEventString 3))
                         |> (fun r -> r.Next.Value(createEventString 4))
                         
            do Expect.contains result.Result { Id = 4 } ""
            do Expect.hasLength result.Result 1 ""
            do Expect.isSome result.Next ""
            
            let result2 = testOrchestration (createEventString 1)
                         |> (fun r -> r.Next.Value(createEventString 2))
                         |> (fun r -> r.Next.Value(createEventString 3))
                         |> (fun r -> r.Next.Value(createEventString 1))
                         |> (fun r -> r.Next.Value(createEventString 2))
                         |> (fun r -> r.Next.Value(createEventString 3))
                         |> (fun r -> r.Next.Value(createEventString 4))
                         
            do Expect.contains result.Result { Id = 4 } ""
            do Expect.isSome result2.Next ""
            
            let result3 = testOrchestration (createEventString 1)
                         |> (fun r -> r.Next.Value(createEventString 2))
                         |> (fun r -> r.Next.Value(createEventString 3))
                         |> (fun r -> r.Next.Value(createEventString 2))
                         |> (fun r -> r.Next.Value(createEventString 4))
                         |> (fun r -> r.Next.Value(createEventString 3))
                         |> (fun r -> r.Next.Value(createEventString 4))
                         
            do Expect.contains result3.Result { Id = 4 } ""
            do Expect.hasLength result3.Result 1 ""
            do Expect.isSome result3.Next ""
                         
            let result4 = testOrchestration (createEventString 1)
                          |> (fun r -> r.Next.Value(createEventString 2))
                          |> (fun r -> r.Next.Value(createEventString 3))
                          |> (fun r -> r.Next.Value(createEventString 1))
                          |> (fun r -> r.Next.Value(createEventString 2))
                          |> (fun r -> r.Next.Value(createEventString 4))
                         
            do Expect.contains result3.Result { Id = 4 } ""
            do Expect.isSome result4.Next ""
            
            let result5 = testOrchestration (createEventString 1)
                         |> (fun r -> r.Next.Value(createEventString 1))
                         |> (fun r -> r.Next.Value(createEventString 2))
                         |> (fun r -> r.Next.Value(createEventString 3))                        
                         |> (fun r -> r.Next.Value(createEventString 2))
                         |> (fun r -> r.Next.Value(createEventString 3))
                         |> (fun r -> r.Next.Value(createEventString 4))
                         
            let list = result5.Result
            do Expect.equal list [{ Id = 4 }] ""
            do Expect.isSome result5.Next ""
        
        testCase "7" <| fun _ ->
            let testOrchestration =
                (eventReceived |> filter (fun x -> x.Id = 1))
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 2) |> take 1)
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 3) |> take 1)
                |> mergeMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 4))
                
            let result = testOrchestration (createEventString 1)
                         |> (fun r -> r.Next.Value(createEventString 2))
                         |> (fun r -> r.Next.Value(createEventString 3))
                         |> (fun r -> r.Next.Value(createEventString 1))
                         |> (fun r -> r.Next.Value(createEventString 2))
                         |> (fun r -> r.Next.Value(createEventString 1))
                         |> (fun r -> r.Next.Value(createEventString 2))
                         |> (fun r -> r.Next.Value(createEventString 3))
                         |> (fun r -> r.Next.Value(createEventString 4))
                         
            let list = result.Result
            do Expect.equal list [{ Id = 4 }; { Id = 4 }] ""
            
        testCase "8" <| fun _ ->
            let testOrchestration =
                (eventReceived |> filter (fun x -> x.Id = 1))
                |> mergeMap (fun _ ->
                    (eventReceived |> filter (fun x -> x.Id = 2))
                    |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 3) |> take 1)
                    |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 4) |> take 1))
                
            let result = testOrchestration (createEventString 1)
                         |> (fun r -> r.Next.Value(createEventString 2))
                         |> (fun r -> r.Next.Value(createEventString 3))
                         |> (fun r -> r.Next.Value(createEventString 1))
                         |> (fun r -> r.Next.Value(createEventString 2))
                         |> (fun r -> r.Next.Value(createEventString 3))
                         |> (fun r -> r.Next.Value(createEventString 4))
                         
            do Expect.equal result.Result [{ Id = 4 }; { Id = 4 }] ""
                         
    ]