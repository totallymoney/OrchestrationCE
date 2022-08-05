module OrchestrationCETests.YieldTests

open Expecto
open OrchestrationCE.Coordination

type Event =
    | Event1
    | Event2

[<Tests>]
let orchestrationCETests =
    testList "Yield tests" [
        testCase "1" <| fun _ ->
            let coord =
                coordination {
                    yield 1
                    yield 2
                    yield 3
                }
                
            let result = (coord ()).Result
            do Expect.equal result [1;2;3] ""
            
        testCase "2" <| fun _ ->
            let coord =
                coordination {
                    yield 1
                    yield 2
                    yield! event (function | Event1 -> Some 3 | _ -> None)
                }
                
            let result = (coord Event1).Result
            do Expect.equal result [1;2;3] ""
            
        testCase "3" <| fun _ ->
            let coord =
                coordination {
                    yield 1
                    yield 2
                    yield! event (function | Event1 -> Some 3 | _ -> None)
                }
                
            let result = (coord Event2).Result
            do Expect.equal result [1;2] ""
            
        testCase "4" <| fun _ ->
            let coord =
                coordination {
                    yield 1
                    yield! event (function | Event1 -> Some 2 | _ -> None)
                    yield 3
                }
                
            let result = (coord Event2).Result
            do Expect.equal result [1] ""
            
        testCase "5" <| fun _ ->
            let coord =
                coordination {
                    yield 1
                    yield! (event (function | Event2 -> Some 2 | _ -> None) |> take 1)
                    yield 3
                }
                
            let result = (coord Event2).Result
            do Expect.equal result [1;2;3] ""
            
        testCase "6" <| fun _ ->
            let coord =
                coordination {
                    let! x = event (function | Event1 -> Some 4 | _ -> None)
                    yield 1
                    yield! (event (function | Event2 -> Some 2 | _ -> None) |> take 1)
                    yield 3
                    return x
                }
                
            let result = (coord Event2).Result
            do Expect.isEmpty result ""
            
            let result2 = (coord Event1).Result
            do Expect.equal result2 [1] ""
            
            let result2 =
                (coord Event1)
                |> fun x -> x.Next.Value Event2
                
            do Expect.equal result2.Result [2;3;4] ""
            
        testCase "7" <| fun _ ->
            let coord =
                coordination {
                    yield! ((event Some) |> takeWhile not)
                    yield! ((event Some) |> (takeWhile (fun x -> x = true)) |> take 1)
                }
                
            let result = (coord true)
            do Expect.equal result.Result [true] ""
            do Expect.isNone result.Next ""
            
            let result2 = (coord false)
            do Expect.equal result2.Result [false] ""
            do Expect.isSome result2.Next ""
        
        testCase "8" <| fun _ ->
            let coord =
                coordination {
                    yield! ((event Some) |> takeWhile not)
                    yield! ((event Some) |> (takeWhile (fun x -> x = true)) |> take 1)
                }
                
            let result = (coord true)
            do Expect.equal result.Result [true] ""
            do Expect.isNone result.Next ""
            
            let result2 = (coord false)
            do Expect.equal result2.Result [false] ""
            do Expect.isSome result2.Next ""
            
            let result3 = (result2.Next.Value false)
            do Expect.equal result3.Result [false] ""
            do Expect.isSome result3.Next ""
            
            let result4 = (result3.Next.Value true)
            do Expect.equal result4.Result [true] ""
            do Expect.isNone result4.Next ""
        ]