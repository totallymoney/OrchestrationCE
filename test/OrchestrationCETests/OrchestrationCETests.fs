module OrchestrationCETests.OrchestrationCETests

open Expecto
open OrchestrationCE
open Coordination
open Orchestration
open TestEvents
open FSharp.Control.Tasks

type Operation =
    | DoThing
    | DoOtherThing
    
[<Tests>]
let orchestrationCETests =
    
    testList "orchestrationCE tests" [
        testCase "1" <| fun _ ->
             let orc = orchestration {
                 let! x = raiseToOrchestrationWithActions
                              [task { return () }]
                              (eventReceived |> filter (fun e -> e.Id = 1))
                              
                              
                 let! y = raiseToOrchestrationWithActions
                              [task { return () }]
                              (eventReceived |> filter (fun e -> e.Id = 2))
                              
                 return y
             }
             
             let res = orc (createEventString 1 |> Some)
                       |> (fun r -> r.Next.Value (createEventString 2 |> Some))
             let list = res.Result
             do Expect.isNone res.Next ""
             do Expect.equal list [(Continue { Id = 2 })] ""
             
             let res2 = orc None
             
             let list2 = res2.Result
             do Expect.isNone res2.Next ""
             
        testCase "2" <| fun _ ->
             let orc = raiseToOrchestration (eventReceived |> filter (fun e -> e.Id = 1))
                       |> switchMap (fun _ ->
                            orchestration {
                                let! x = raiseToOrchestrationWithActions
                                             [DoThing]
                                             (eventReceived |> filter (fun e -> e.Id = 2))
                                             
                                let! y = raiseToOrchestration
                                             (eventReceived |> filter (fun e -> e.Id = x.Id + 1))
                                             
                                let! z = raiseToOrchestrationWithActions
                                             [DoOtherThing]
                                             (eventReceived |> filter (fun e -> e.Id = y.Id + 1))
                                             
                                return z
                            })
             
             let res = orc (createEventString 1 |> Some)
                       |> (fun r -> r.Next.Value None)
                       
             let list = res.Result
             do Expect.equal list [(Break [DoThing])] ""
             
             let res2 = orc (createEventString 1 |> Some)
                        |> (fun r -> r.Next.Value (createEventString 2 |> Some))
                        |> (fun r -> r.Next.Value (createEventString 3 |> Some))
                        |> (fun r -> r.Next.Value None)
             
             let list2 = res2.Result
             do Expect.equal list2 [(Break [DoOtherThing])] ""
             
             let res3 = orc (createEventString 1 |> Some)
                        |> (fun r -> r.Next.Value (createEventString 2 |> Some))
                        |> (fun r -> r.Next.Value (createEventString 3 |> Some))
                        |> (fun r -> r.Next.Value (createEventString 1 |> Some))
                        |> (fun r -> r.Next.Value (createEventString 4 |> Some))
             
             do Expect.isEmpty res3.Result ""
             
             let res4 = orc (createEventString 1 |> Some)
                        |> (fun r -> r.Next.Value (createEventString 2 |> Some))
                        |> (fun r -> r.Next.Value (createEventString 3 |> Some))
                        |> (fun r -> r.Next.Value (createEventString 1 |> Some))
                        |> (fun r -> r.Next.Value (createEventString 2 |> Some))
                        |> (fun r -> r.Next.Value (createEventString 3 |> Some))
                        |> (fun r -> r.Next.Value (createEventString 4 |> Some))
             
             let list3 = res4.Result
             do Expect.equal list3 [(Continue { Id = 4 })] ""
             
        testCase "3" <| fun _ ->
             let orc = raiseToOrchestration (eventReceived |> filter (fun e -> e.Id = 1))
                       |> mergeMap (fun _ ->
                            orchestration {
                                let! x = raiseToOrchestrationWithActions
                                             [DoThing]
                                             (eventReceived |> filter (fun e -> e.Id = 2))
                                             
                                let! y = raiseToOrchestration (eventReceived |> filter (fun e -> e.Id = x.Id + 1))
                                let! z = raiseToOrchestrationWithActions
                                             [DoOtherThing]
                                             (eventReceived |> filter (fun e -> e.Id = y.Id + 1))
                                             
                                return z
                            })
                       
             let res = orc (createEventString 1 |> Some)
                        |> (fun r -> r.Next.Value (createEventString 2 |> Some))
                        |> (fun r -> r.Next.Value (createEventString 3 |> Some))
                        |> (fun r -> r.Next.Value (createEventString 1 |> Some))
                        |> (fun r -> r.Next.Value (createEventString 2 |> Some))
                        |> (fun r -> r.Next.Value (createEventString 3 |> Some))
                        |> (fun r -> r.Next.Value (createEventString 4 |> Some))
                       
             let list = res.Result
             do Expect.equal list [(Continue { Id = 4 }); (Continue { Id = 4 })] ""
             
        testCase "4" <| fun _ ->
            let orc = coordination {
                        let! x = (eventReceived |> filter (fun x -> x.Id = 1))
                        and! y = (eventReceived |> filter (fun x -> x.Id = 2))
                        and! z = (eventReceived |> filter (fun x -> x.Id = 3))
                        return x
                      }
            
            let res = orc (createEventString 1)
                      |> (fun r -> r.Next.Value (createEventString 2))
                      |> (fun r -> r.Next.Value (createEventString 1))
                      |> (fun r -> r.Next.Value (createEventString 3))
                      
            do Expect.equal res.Result [{ Id = 1 }] ""
            do Expect.isNone res.Next ""
            
            let res2 = orc (createEventString 1)
                      |> (fun r -> r.Next.Value (createEventString 2))
                      |> (fun r -> r.Next.Value (createEventString 1))
                      
            do Expect.isEmpty res2.Result ""
            do Expect.isSome res2.Next ""
            
            let res3 = orc (createEventString 1)
                      |> (fun r -> r.Next.Value (createEventString 3))
                      |> (fun r -> r.Next.Value (createEventString 2))

            do Expect.equal res3.Result [{ Id = 1 }] ""
            do Expect.isNone res3.Next ""
            
        testCase "5" <| fun _ ->
            let orc =
                orchestration {
                    let! r = raiseToOrchestrationWithActions
                                [DoThing]
                                (coordination {
                                    let! a = (eventReceived |> filter (fun x -> x.Id = 1))
                                    and! b = (eventReceived |> filter (fun x -> x.Id = 2))
                                    and! c = (eventReceived |> filter (fun x -> x.Id = 3))
                                    return a
                                })
                                
                    let! r2 = raiseToOrchestrationWithActions
                                [DoOtherThing]
                                (coordination {
                                    let! a = (eventReceived |> filter (fun x -> x.Id = 4))
                                    and! b = (eventReceived |> filter (fun x -> x.Id = 5))
                                    and! c = (eventReceived |> filter (fun x -> x.Id = 6))
                                    return a
                                })
                                
                    return r2.Id
                }
            
            let res = orc None
                      
            do Expect.equal res.Result [Break [DoThing]] ""
            
            let res2 =
                orc (createEventString 1 |> Some)
                |> (fun r -> r.Next.Value (createEventString 2 |> Some))
                |> (fun r -> r.Next.Value (createEventString 1 |> Some))
                
            do Expect.isEmpty res2.Result ""
            do Expect.isSome res2.Next ""
            
            let res3 =
                orc (createEventString 1 |> Some)
                |> (fun r -> r.Next.Value (createEventString 2 |> Some))
                |> (fun r -> r.Next.Value None)
                
            do Expect.equal res3.Result [Break [DoThing]] ""
            do Expect.isNone res3.Next ""
            
            let res4 =
                orc (createEventString 1 |> Some)
                |> (fun r -> r.Next.Value (createEventString 2 |> Some))
                |> (fun r -> r.Next.Value (createEventString 3 |> Some))
                |> (fun r -> r.Next.Value None)
                
            do Expect.equal res4.Result [Break [DoOtherThing]] ""
            do Expect.isNone res4.Next ""
            
            let res5 =
                orc (createEventString 1 |> Some)
                |> (fun r -> r.Next.Value (createEventString 2 |> Some))
                |> (fun r -> r.Next.Value (createEventString 3 |> Some))
                |> (fun r -> r.Next.Value (createEventString 4 |> Some))
                |> (fun r -> r.Next.Value (createEventString 5 |> Some))
                |> (fun r -> r.Next.Value (createEventString 6 |> Some))
                
            do Expect.equal res5.Result [Continue 4] ""
            do Expect.isNone res5.Next ""
            
        testCase "6" <| fun _ ->
            let orc =
                orchestration {
                    let! r = raiseToOrchestrationWithActions
                                 [DoThing]
                                 (eventReceived |> filter (fun e -> e.Id = 1))
                                 
      
                    and! r2 = raiseToOrchestrationWithActions
                                [DoOtherThing]
                                (eventReceived |> filter (fun x -> x.Id = 2))
                                
                                
                    and! r3 = raiseToOrchestrationWithActions
                                [DoOtherThing]
                                (eventReceived |> filter (fun x -> x.Id = 3))
                                

                    return r3.Id
                }
                
            let res = orc None
            do Expect.equal res.Result [Break [DoThing; DoOtherThing; DoOtherThing]] ""
            do Expect.isNone res.Next ""
            
            let res2 =
                orc (createEventString 1 |> Some)
                |> (fun r -> r.Next.Value None)
            
            do Expect.equal res2.Result [Break [DoOtherThing; DoOtherThing]] ""
            do Expect.isNone res2.Next ""

            let res3 =
                orc (createEventString 2 |> Some)
                |> (fun r -> r.Next.Value None)

            do Expect.equal res3.Result [Break [DoThing; DoOtherThing]] ""
            do Expect.isNone res3.Next ""
            
            let res4 =
                orc (createEventString 1 |> Some)
                |> (fun r -> r.Next.Value (createEventString 3 |> Some))
                |> (fun r -> r.Next.Value (createEventString 2 |> Some))
                
            do Expect.equal res4.Result [Continue 3] ""
            do Expect.isNone res4.Next ""
            ]
    

