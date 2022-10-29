namespace FSChatServer.Library

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Threading
open Interface
open Storage
open OldProtocol

module Server =
    open System.Diagnostics    
    open System.Collections.Generic
    open System.Text


    let LoginErrorUserIdExists user: Result<User> = Failed(sprintf "handle %s already exists" user.handle)
    let LoginErrorNoUserId = Failed(sprintf "handle is required")

    let validateUser getUser sessionId handle =
        match String.IsNullOrWhiteSpace(handle) with
        | true -> LoginErrorNoUserId
        | _ ->
            match getUser(handle) with
            | Some user -> LoginErrorUserIdExists(user)
            | _ -> Success ({ handle = handle; sessionId = sessionId })

    let GetUserListNoUsers = Notice("No users are registered.")
        
    let getCurrentUsers (getUsers:GetUserList) =
        let users = getUsers()
        match users with
        | [] -> GetUserListNoUsers
        | _ -> Data(UserList users)

    let sessionManager = 
        let sm = MailboxProcessor.Start(fun inbox ->            
           
            let sessions = Dictionary<Guid, MailboxProcessor<ClientCommand>>()
            
            let database = inMemoryUserService(new ResizeArray<User>())

            let sendBroadcast sessionId clientCommand =                 
                sessions.Keys
                |> Seq.filter (fun key -> key <> sessionId)
                |> Seq.iter (fun key -> sessions.[key].Post(clientCommand))

            let rec messageLoop () = async {
                
                let! command = inbox.Receive()

                printf "%A\n" command

                match command with
//                | Ping ->
//                    sessions.Keys
//                    |> Seq.iter (fun key -> sessions.[key].Post(ClientCommand.Ping))

                | AddSession (sessionId, stream) -> sessions.Add(sessionId, stream)
                | RemoveSession (sessionId) ->
                    match database.getUserById(sessionId) with
                    | Some (user) ->
                        sendBroadcast sessionId (DeleteUser(user.handle))
                        database.removeUser(user) |> ignore //don't care about failure
                    | _ -> ()                    
                    sessions.Remove(sessionId) |> ignore
                | SendCommand (sessionId, serverCommand) ->
                    let res =                                                 
                        match serverCommand with
                        | Success sc ->
                            let authenticationRequired =             
                                if (requiresAuthentication sc) then
                                    let user = database.getUserById(sessionId)
                                    user.IsNone
                                else
                                    false
                            
                            if authenticationRequired then
                                Unauthenticated
                            else
                                match sc with
                                | Login (userId) ->                                
                                    match (validateUser database.getUser sessionId userId) with
                                    | Success (user) -> 
                                        match database.addUser(user) with
                                        |Success (user) ->
                                            sendBroadcast sessionId (AddUser(user))
                                            OK
                                        | Failed (msg) -> Error(msg)
                                    | Failed (msg) -> Error(msg)                            
                                | ServerCommand.ThisGuy -> Data ServerDataResult.ThisGuy
                                | GetUserList -> getCurrentUsers database.getUserList                        
                                | Logout -> 
                                    match database.getUserById(sessionId) with
                                    | Some (user) ->
                                        sendBroadcast sessionId (DeleteUser(user.handle))
                                        database.removeUser(user) |> ignore //don't care about failure
                                    | _ -> ()
                                    Stop

                                | SendMessage(toUserId, message) -> 
                                    match database.getUser(toUserId) with
                                    | None -> Error("User " + toUserId + " not found")
                                    | Some(sendToUser) ->
                                        match database.getUserById(sessionId) with
                                        | None -> Error("Session not found")
                                        | Some(fromUser)  ->
                                            sessions.[sendToUser.sessionId].Post(ReceivedMessage( { fromUserId = fromUser.handle; toUserId = toUserId; message = message }))
                                            OK
                                                
                        | Failed (msg) -> Error(msg)
                    
                    sessions.[sessionId].Post(res)                      

                return! messageLoop()
            }
            messageLoop()
        )
        sm.Error.Add(fun ex -> printfn "SESSION EXCEPTION: %s" ex.Message)
        sm
    
    let createClientMailbox (stream:StreamWriter) =
        let mb = MailboxProcessor.Start(fun inbox ->
            let rec messageLoop () = async {
                //let! (response:string) = inbox.Receive()
                let! command = inbox.Receive()
                if command <> Stop then
                    let response = OldProtocol.getClientCommand(command)
                    stream.WriteLine(response)
                    stream.Flush()
                
                    return! messageLoop()
            }
        
            messageLoop()
        )
        mb.Error.Add(fun ex -> printfn "EXCEPTION: %s" ex.Message)
        mb

    let createClientMailboxWS (sessionId:Guid) (ns:NetworkStream) =
        let mb = MailboxProcessor.Start(fun inbox ->
            let rec messageLoop () = async {
                
                let! command = inbox.Receive()
                if command <> Stop then
                    let frame =
                        command
                        |> WebSocketProtocol.getClientCommand 
                        |> Encoding.UTF8.GetBytes
                        |> WebSocketProtocol.makeFrame                    

                    let response = 
                        frame
                        |> WebSocketProtocol.writeFrameNew                    

                    do! ns.AsyncWrite(response, 0, response.Length)
                
                    return! messageLoop()
            }
        
            messageLoop()
        )
        mb.Error.Add(fun ex -> 
            printfn "EXCEPTION: %s" ex.Message
            sessionManager.Post(RemoveSession(sessionId))
            )        
        mb

    let runClient sessionId (client:TcpClient) = async {
        use stream = client.GetStream()        
        use reader = new StreamReader(stream) 
        use writer = new StreamWriter(stream)        

        try
            writer.WriteLine("Welcome to INTERCHAT!");
            writer.WriteLine("Your session id: " + sessionId.ToString());
            writer.Flush()

            let rec loop header =            
                                              
                let command = OldProtocol.getServerCommand header            
            
                sessionManager.Post(SendCommand(sessionId, command))
                                                              
                match command with
                | Success ServerCommand.Logout -> ()
                | _ -> loop (reader.ReadLine())

            loop (reader.ReadLine())            

            client.Close()
        with ex -> 
            printfn "RUN ERROR: %s" (ex.ToString())

        sessionManager.Post(RemoveSession(sessionId))
       
    }


    let runClientWS sessionId (client:TcpClient) = async {        
        try

            let loop () = async {
                use stream = client.GetStream()
                let mutable keepRunning = true
                while keepRunning do
                    let bytes = Array.create client.ReceiveBufferSize (byte 0)
                    
                    let! bytesReadCount = stream.ReadAsync (bytes, 0, bytes.Length) |> Async.AwaitTask

                    if bytesReadCount > 0 then                    
                        let frame = WebSocketProtocol.readFrame bytes

                        match frame.opCode with
                        | 8 -> keepRunning <- false
                        | 1 ->
                            
                            let decodedData = (WebSocketProtocol.decodeMaskedData frame.maskingKey frame.payload) |> System.Text.UTF8Encoding.UTF8.GetString
                            

                            let command = WebSocketProtocol.getServerCommand decodedData
            
                            sessionManager.Post(SendCommand(sessionId, command))
                        
                            if command = (Success ServerCommand.Logout) then
                                keepRunning <- false
                        | _ -> ()                                       

                sessionManager.Post(RemoveSession(sessionId))
                client.Close()
            }                                          
            loop () |> Async.Start
            

        with ex -> 
            printfn "RUN WS ERROR: %s" (ex.ToString())      
            sessionManager.Post(RemoveSession(sessionId))
       
    }

    let startServer (ip, port) =        
        let listener = TcpListener(ip, port)
        listener.Start() 
                       
        async { 
            while true do 
                let! client = listener.AcceptTcpClientAsync() |> Async.AwaitTask
                let sessionId = Guid.NewGuid()
                let clientMailbox = createClientMailbox (new StreamWriter(client.GetStream()))                             
                
                sessionManager.Post(AddSession(sessionId, clientMailbox))                

                runClient sessionId client |> Async.Start
        }
        |> Async.Start


    let startServerWS (ip, port) =        
        let listener = TcpListener(ip, port)
        listener.Start() 

        let handshake (tcp:TcpClient) = async {                          
            //this code is borrowed from
            //https://gist.github.com/jonschoning/14f3a087b2bb79236131
            let ns = tcp.GetStream()
            let bytes = Array.create tcp.ReceiveBufferSize (byte 0)
            let bytesReadCount = ns.Read (bytes, 0, bytes.Length)
            
            if bytesReadCount > 8 then                
                let lines = bytes.[..(bytesReadCount)]
                            |> System.Text.UTF8Encoding.UTF8.GetString 
                            |> fun hs->hs.Split([|"\r\n"|], StringSplitOptions.RemoveEmptyEntries) 
                match WebSocketProtocol.isWebSocketsUpgrade lines with
                | true ->
                    let acceptStr =  
                                    (WebSocketProtocol.getKey "Sec-WebSocket-Key:" lines                 
                                    ).Substring(1)                                // skip space
                                    |> WebSocketProtocol.calcWSAccept6455 
                                    |> WebSocketProtocol.createAcceptString6455
                    Console.WriteLine acceptStr                                   // debug
                    do! ns.AsyncWrite <| Encoding.ASCII.GetBytes acceptStr
                    return true
                | _ ->                      
                    tcp.Close()  
                    return false                                                    // validation failed - close connection
            else
                tcp.Close()
                return false   
            
                                                                // validation failed - close connection
            }

                       
        async { 
            while true do 
                let! client = listener.AcceptTcpClientAsync() |> Async.AwaitTask
                let sessionId = Guid.NewGuid()

                let! isWSConnection = handshake client

                if isWSConnection then
                    let clientMailbox = createClientMailboxWS (sessionId) (client.GetStream())                             
                
                    sessionManager.Post(AddSession(sessionId, clientMailbox))                

                    runClientWS sessionId client |> Async.Start
        }
        |> Async.Start
                       
    let logoutUser = OK
    
    let handleCommand (functions:CommandHandlerFunctions) (sessionId:Guid, command:ServerCommand): ClientCommand = 
        Error("not implemented")          

//        let handleLogin handle =            
//            let steps = 
//                (validateUser functions.getUser) 
//                >> bind (loginUser sendClient functions.addUser sessionId)
//
//            match steps handle with
//            | Success _ -> OK
//            | Failed msg -> Error ("Failed to login: " + msg)

//        let authenticationRequired =             
//            if (requiresAuthentication command) then
//                let user = functions.getUserById(sessionId)
//                user.IsNone
//            else
//                false
//
//        if (authenticationRequired) then
//            Unauthenticated
//        else        
//            match command with
//            | ServerCommand.ThisGuy -> Data ServerDataResult.ThisGuy
//            | GetUserList -> getCurrentUsers functions.getUserList
//            | Login(handle) -> OK //handleLogin (handle)
//            | Logout -> logoutUser
//            | SendMessage(userId, message) -> ReceivedMessage("todo", userId, message) //sendMessage sendClient (userId, message)
                            