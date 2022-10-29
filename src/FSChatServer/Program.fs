// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.Threading
open FSChatServer.Library.Server
open FSChatServer.Library.Interface
open FSChatServer.Library.Storage
open System.Net

[<EntryPoint>]
let main argv = 
    printfn "%A" argv


    startServer (IPAddress.Any, 1337)
    startServerWS (IPAddress.Any, 1900)

    Console.ReadLine() |> ignore

    printfn "bye!"
    
    0 // return an integer exit code
