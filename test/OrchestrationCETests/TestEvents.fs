module OrchestrationCETests.TestEvents

open System.Text.Json
open System.Text.Json.Serialization
open OrchestrationCE
open Workflow

type Event = string option

let tryDeserializeJson<'a> (serialized: string) =
    try
        let options = JsonSerializerOptions()
        options.Converters.Add(JsonFSharpConverter())
        JsonSerializer.Deserialize<'a>(serialized, options)
        |> Some
    with
        | _ -> None
                            
let rec eventReceived<'a> event = 
    let result = event
                 |> tryDeserializeJson<'a> 
                 |> Option.toList
    { Result = result; Next = Some eventReceived<'a>  }
    
type TestEvent =
    { Id: int }
    
let createEventString id =
    let options = JsonSerializerOptions()
    options.Converters.Add(JsonFSharpConverter())
    JsonSerializer.Serialize({ Id = id }, options)
    
let serialize str =
    let options = JsonSerializerOptions()
    options.Converters.Add(JsonFSharpConverter())
    JsonSerializer.Serialize(str, options)