module OrchestrationCETests.SwitchMapTests

open OrchestrationCE
open Expecto
open Workflow
open TestEvents

[<Tests>]
let switchMapTests =
    testList "switchMap tests" [
        testCase "1" <| fun _ ->
            let testOrchestration =
                (eventReceived |> filter (fun x -> x.Id = 1)) 
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 1))
                
            let event = createEventString 1
            let result = testOrchestration event
            do Expect.contains result.Result { Id = 1 } ""
            do Expect.hasLength result.Result 1 ""
            do Expect.isSome result.Next ""
            
            let result2 = result.Next.Value event
            do Expect.contains result2.Result { Id = 1 } ""
            do Expect.hasLength result2.Result 1 ""
            do Expect.isSome result2.Next ""
            
            let result3 = result.Next.Value event
            do Expect.contains result3.Result { Id = 1 } ""
            do Expect.hasLength result3.Result 1 ""
            do Expect.isSome result3.Next ""
            
        testCase "2" <| fun _ ->
            let testOrchestration =
                eventReceived |> filter (fun x -> x.Id = 1) |> take 1
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 1) |> take 1)
                
            let event = createEventString 1
            let result = testOrchestration event
            do Expect.contains result.Result { Id = 1 } ""
            do Expect.hasLength result.Result 1 ""
            do Expect.isNone result.Next ""
            
        testCase "3" <| fun _ ->
            let testOrchestration =
                eventReceived |> filter (fun x -> x.Id = 1)
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 1) |> take 1)
                
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
                eventReceived |> filter (fun x -> x.Id = 1) |> take 1
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 1) )
                
            let event = createEventString 1
            let result = testOrchestration event
            do Expect.contains result.Result { Id = 1 } ""
            do Expect.hasLength result.Result 1 ""
            do Expect.isSome result.Next ""
            
            let result2 = result.Next.Value event
            do Expect.contains result2.Result { Id = 1 } ""
            do Expect.hasLength result2.Result 1 ""
            do Expect.isSome result2.Next ""
            
            let result3 = result.Next.Value event
            do Expect.contains result3.Result { Id = 1 } ""
            do Expect.hasLength result3.Result 1 ""
            do Expect.isSome result3.Next ""
        
        testCase "5" <| fun _ ->
            let testOrchestration =
                (eventReceived |> filter (fun x -> x.Id = 1))
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 2))
            
            let event = createEventString 1
            let result = testOrchestration event
            do Expect.isEmpty result.Result ""
            do Expect.isSome result.Next ""
            
            let result2 = result.Next.Value event
            do Expect.isEmpty result2.Result ""
            do Expect.isSome result2.Next ""
            
            let event2 = createEventString 2
            let result3 = result.Next.Value event2
            do Expect.contains result3.Result { Id = 2 } ""
            do Expect.hasLength result3.Result 1 ""
            do Expect.isSome result3.Next ""
            
            let result4 = testOrchestration event2
            do Expect.isEmpty result4.Result ""
            do Expect.isSome result4.Next ""
        
        testCase "6" <| fun _ ->
            let testOrchestration =
                eventReceived |> filter (fun x -> x.Id = 1)
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 2) |> take 1)
                
            let event = createEventString 1
            let result = testOrchestration event
            do Expect.isEmpty result.Result ""
            do Expect.isSome result.Next ""
            
            let event2 = createEventString 2
            let result2 = result.Next.Value event2
            do Expect.contains result2.Result { Id = 2 } ""
            do Expect.isSome result2.Next ""
            
            let result3 = result2.Next.Value event2
            do Expect.isEmpty result3.Result ""
            do Expect.isSome result3.Next ""
            
            let result4 = result3.Next.Value event
            do Expect.isEmpty result4.Result ""
            do Expect.isSome result4.Next ""
            
            let result5 = result4.Next.Value event2
            do Expect.contains result5.Result { Id = 2 } ""
            do Expect.isSome result5.Next ""
            
        testCase "7" <| fun _ ->
            let testOrchestration =
                (eventReceived |> filter (fun x -> x.Id = 1))
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 2) |> take 1)
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 3) |> take 1)
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 4) |> take 1)
                
            let result = testOrchestration (createEventString 1)
                         |> (fun r -> r.Next.Value(createEventString 2))
                         |> (fun r -> r.Next.Value(createEventString 3))
                         |> (fun r -> r.Next.Value(createEventString 4))
            
            do Expect.contains result.Result { Id = 4 } ""
            do Expect.isSome result.Next ""

            let result2 = testOrchestration (createEventString 1)
                          |> (fun r -> r.Next.Value(createEventString 2))
                          |> (fun r -> r.Next.Value(createEventString 3))
                          |> (fun r -> r.Next.Value(createEventString 7))
                          |> (fun r -> r.Next.Value(createEventString 4))
                         
            do Expect.contains result2.Result { Id = 4 } ""
            do Expect.isSome result2.Next ""
            
            let result3 = testOrchestration (createEventString 1)
                          |> (fun r -> r.Next.Value(createEventString 2))
                          |> (fun r -> r.Next.Value(createEventString 3))
                          |> (fun r -> r.Next.Value(createEventString 3))
                          |> (fun r -> r.Next.Value(createEventString 4))
                         
            do Expect.contains result3.Result { Id = 4 } ""
            do Expect.isSome result3.Next ""
            
            // switchMap does not behave the same if the switchmaps are in a line as opposed to nexted (see test 10).
            // This is consistent with rxjs.
            let result4 = testOrchestration (createEventString 1)
                          |> (fun r -> r.Next.Value(createEventString 2))
                          |> (fun r -> r.Next.Value(createEventString 3))
                          |> (fun r -> r.Next.Value(createEventString 1))
                          |> (fun r -> r.Next.Value(createEventString 4))   
                          
            do Expect.contains result4.Result { Id = 4 } ""
            do Expect.isSome result4.Next ""
            
            let result5 = result4
                          |> (fun r -> r.Next.Value(createEventString 2))
                          |> (fun r -> r.Next.Value(createEventString 3))
                          |> (fun r -> r.Next.Value(createEventString 4))
                          
            do Expect.contains result5.Result { Id = 4 } ""
            do Expect.isSome result5.Next ""
            
        testCase "8" <| fun _ ->
            let testOrchestration =
                eventReceived |> filter (fun x -> x.Id = 1) |> take 2
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 2) |> take 1)
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 3) |> take 1)
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 4) |> take 1)
            
            let result = testOrchestration (createEventString 1)
                         |> (fun r -> r.Next.Value(createEventString 2))
                         |> (fun r -> r.Next.Value(createEventString 3))
                         |> (fun r -> r.Next.Value(createEventString 4))
            
            do Expect.contains result.Result { Id = 4 } ""
            do Expect.isSome result.Next ""
            
            // switchMap does not behave the same if the switchmaps are in a line as opposed to nexted (see test 10).
            // This is consistent with rxjs.
            let result2 = testOrchestration (createEventString 1)
                         |> (fun r -> r.Next.Value(createEventString 2))
                         |> (fun r -> r.Next.Value(createEventString 3))
                         |> (fun r -> r.Next.Value(createEventString 1))
                         |> (fun r -> r.Next.Value(createEventString 2))
                         |> (fun r -> r.Next.Value(createEventString 4))
                          
            do Expect.contains result2.Result { Id = 4 } ""
            do Expect.isSome result2.Next ""
            
            let result3 = result2
                          |> (fun r -> r.Next.Value(createEventString 3))
                          |> (fun r -> r.Next.Value(createEventString 4))
                          
            do Expect.contains result3.Result { Id = 4 } ""
            do Expect.isNone result3.Next ""
            
        testCase "9" <| fun _ ->
            let testOrchestration =
                eventReceived |> filter (fun x -> x.Id = 1) |> take 1
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 2) |> take 2)
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 3) |> take 1)
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 4) |> take 1)
                
            let result = testOrchestration (createEventString 1)
                         |> (fun r -> r.Next.Value(createEventString 2))
                         |> (fun r -> r.Next.Value(createEventString 3))
                         |> (fun r -> r.Next.Value(createEventString 4))
            
            do Expect.contains result.Result { Id = 4 } ""
            do Expect.isSome result.Next ""
            
            let result2 = result.Next.Value(createEventString 3)
            do Expect.isEmpty result2.Result ""
            do Expect.isSome result2.Next ""
            
            let result3 = result2
                          |> (fun r -> r.Next.Value(createEventString 2))
                          |> (fun r -> r.Next.Value(createEventString 3))
                          |> (fun r -> r.Next.Value(createEventString 4))
                       
            do Expect.contains result3.Result { Id = 4 } ""
            do Expect.isNone result3.Next ""
            
    
        testCase "10" <| fun _ ->
            let testOrchestration =
                (eventReceived |> filter (fun x -> x.Id = 2) |> take 1)
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 3) |> take 1)
                |> switchMap (fun _ -> eventReceived |> filter (fun x -> x.Id = 4) |> take 1)
                
            let testOrchestration2 =
                (eventReceived |> filter (fun x -> x.Id = 1))
                |> switchMap (fun _ -> testOrchestration)
                
            let result4 = testOrchestration2 (createEventString 1)
                          |> (fun r -> r.Next.Value(createEventString 2))
                          |> (fun r -> r.Next.Value(createEventString 3))
                          |> (fun r -> r.Next.Value(createEventString 1))
                          |> (fun r -> r.Next.Value(createEventString 4))   
                          
            do Expect.isEmpty result4.Result ""
            do Expect.isSome result4.Next ""
            ]
   