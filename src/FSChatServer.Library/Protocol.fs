namespace FSChatServer.Library

open System
open Interface

module OldProtocol =       
    

    let parseData (data:string) =
        let index = data.IndexOf('\t')
        match index with
        | -1 -> (data, String.Empty) //assume it's a single command
        | _ -> (data.Substring(0, index), data.Substring(index+1).Trim())


    
    let getServerCommand (data:string): Result<ServerCommand> =                
        if (isNull data) then            
            Success(Logout) //assume disconnected
        else
            try                
                let (command, text) = parseData data
                let items = text.Split([|'\t'|])
                match command.ToLower() with
                | "this_guy" -> Success(ServerCommand.ThisGuy)
                | "login" -> Success(Login(text))
                | "disconnect" -> Success(Logout)
                | "send" -> Success(SendMessage(items.[0], items.[1]))
                | "get_user_list" -> Success(GetUserList)
                | _ -> Failed("Command not recognised")
            with ex ->
                Failed ("Invalid command structure " + data)

    let getUserList (users:User list) =                
        let accFunc (acc:string) (item:User) = 
            acc + (sprintf "D\t%s\t%s\r\n" item.handle item.handle )
        (List.fold accFunc "H\tLOGIN_NAME\tNICK_NAME\r\n" users)
        + "E"

    let getClientCommand (command:ClientCommand) =
        match command with
        | OK -> "OK"        
        | Data data ->
            match data with            
            | ThisGuy -> "THIS_GUY"
            | UserList users -> getUserList users
        | Error msg -> "ERROR\t" + msg //"ERROR\t" + msg
        | Notice msg -> "NOTICE\t" + msg
        | Unauthenticated -> "ERROR\t\r\nSEPT 22,\t18:32:21 PM\r\n\tYOUR ACCESS SUSPENDED\r\n\tPLEASE REPORT TO DILLINGER\r\n\tIMMEDIATELY\r\n\tAUTHORIZATION: MASTER CONTROL\r\n\tPROGRAM\r\n\r\n\tEND OF LINE"
        | AddUser user -> "ADD_USER\t"+user.handle
        | DeleteUser handle -> "DELETE_USER\t"+handle                
        | ReceivedMessage (data) -> (sprintf "MSG\t%s\t%s" data.fromUserId data.message)
        | Stop -> String.Empty


module AnsiProtocol =

    let parseData (data:string) =
        let index = data.IndexOf('\t')
        match index with
        | -1 -> (data, String.Empty) //assume it's a single command
        | _ -> (data.Substring(0, index), data.Substring(index+1).Trim())


    
    let getServerCommand (data:string): Result<ServerCommand> =                
        if (isNull data) then            
            Success(Logout) //assume disconnected
        else
            try                
                let (command, text) = parseData data
                let items = text.Split([|'\t'|])
                match command.ToLower() with
                | "this_guy" -> Success(ServerCommand.ThisGuy)
                | "login" -> Success(Login(text))
                | "disconnect" -> Success(Logout)
                | "send" -> Success(SendMessage(items.[0], items.[1]))
                | "get_user_list" -> Success(GetUserList)
                | _ -> Failed("Command not recognised")
            with ex ->
                Failed ("Invalid command structure " + data)

    let getUserList (users:User list) =                
        let accFunc (acc:string) (item:User) = 
            acc + (sprintf "D\t%s\t%s\r\n" item.handle item.handle )
        (List.fold accFunc "H\tLOGIN_NAME\tNICK_NAME\r\n" users)
        + "E"

//    let getServerResponse (response:ServerResponse) =
//        match response with        
//        | OK -> "OK"
//        | Data data ->
//            match data with            
//            | ThisGuy -> "THIS_GUY"
//            | UserList users -> getUserList users
//        | Error msg -> "ERROR\t" + msg
//        | Notice msg -> "NOTICE\t" + msg
//        | Unauthenticated -> "ERROR\tYou must LOGIN first. Usage: LOGIN[tab]login_name"
        //| UserList(users) -> getUserList users

    let getClientCommand (command:ClientCommand) =
        match command with
        | OK -> ""        
        | Data data ->
            match data with            
            | ThisGuy -> "THIS_GUY"
            | UserList users -> getUserList users
        | Error msg -> "\x1b[0;31mERROR! " + msg
        | Notice msg -> "NOTICE\t" + msg
        | Unauthenticated -> "ERROR\tYou must LOGIN first. Usage: LOGIN[tab]login_name"
        | AddUser user -> "ADD_USER\t"+user.handle
        | DeleteUser handle -> "DELETE_USER\t"+handle                
        | ReceivedMessage (data) -> (sprintf "MSG\t%s\t%s" data.fromUserId data.message)
        | Stop -> String.Empty
    
module WebSocketProtocol =
    //this code is borrowed from
    //https://gist.github.com/jonschoning/14f3a087b2bb79236131
    open Newtonsoft.Json
    open System.IO
    open BitSyntax
    open System.Security.Cryptography
    open System.Text

    let guid6455 = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

  /// headers can occur in any order (See 1.3 in [1])
    let isWebSocketsUpgrade (lines: string array) = 
        [| "GET / HTTP/1.1"; "Upgrade: websocket"|] 
        |> Array.map(fun x->lines |> Array.exists(fun y->x.ToLower()=y.ToLower()))
        |> Array.reduce(fun x y->x && y)
  
  /// see: [1]
    let calcWSAccept6455 (secWebSocketKey:string) = 
        let ck = secWebSocketKey + guid6455
        let sha1 = SHA1CryptoServiceProvider.Create()
        let hashBytes = ck |> Encoding.ASCII.GetBytes |> sha1.ComputeHash
        let Sec_WebSocket_Accept = hashBytes |> Convert.ToBase64String
        Sec_WebSocket_Accept

    let createAcceptString6455 acceptCode = 
        "HTTP/1.1 101 Switching Protocols\r\n" +
        "Upgrade: websocket\r\n" +
        "Connection: Upgrade\r\n" +
        "Sec-WebSocket-Accept: " + acceptCode + "\r\n" 
        + "\r\n"

    let getKey (key:string) arr = 
        try 
            let item = (Array.find (fun (s:String) -> s.StartsWith(key)) arr)
            item.Substring key.Length
        with
        | _ -> ""


    let getClientCommand (command:ClientCommand) = 
        command |> JsonConvert.SerializeObject

    let getServerCommand (data:string): Result<ServerCommand> =
        try
            JsonConvert.DeserializeObject<ServerCommand>(data) 
            |> Success
        with ex -> Failed(ex.Message)


    let decodeMaskedData (mask:byte array) (data:byte array) = data |> Array.mapi (fun index encoded -> encoded ^^^ mask.[index % 4])

    type frame = {
        fin: bool
        rsv1: bool
        rsv2: bool
        rsv3: bool
        opCode: int32        
        masked: bool
        payLoadLength: uint8 //uint16
        payloadLengthExtended: byte array
        payloadLengthActual: uint64
        maskingKey: byte array
        payload: byte array
    }    

    let readFrame (P:byte array) =

        use ms = new MemoryStream(P)

        bitReader ms {                        

            let! fin = BitReader.ReadBool()
            let! rsv1 = BitReader.ReadBool()
            let! rsv2 = BitReader.ReadBool()
            let! rsv3 = BitReader.ReadBool()
            let! opCode = BitReader.ReadInt32(numBits = 4)
            let! masked = BitReader.ReadBool()
            let! payLoadLength = BitReader.ReadByte(numBits = 7)


            let! payLoadLengthExtended =  
                if payLoadLength < 126uy then
                    BitReader.ReadBytes(0)
                elif payLoadLength = 126uy then
                    BitReader.ReadBytes(2)
                else
                    BitReader.ReadBytes(8)

            Array.Reverse(payLoadLengthExtended)

            let payLoadLengthActual = 
                match payLoadLengthExtended.Length with
                | 2 -> BitConverter.ToUInt16(payLoadLengthExtended, 0) |> Convert.ToUInt64
                | 8 -> BitConverter.ToUInt64(payLoadLengthExtended, 0)
                | _ -> payLoadLength |> Convert.ToUInt64

            let! mask = BitReader.ReadBytes(if masked then 4 else 0)

            let! payloadEncoded = BitReader.ReadBytes(numBytes = Convert.ToInt32(payLoadLengthActual))                        
                
            return { 
                fin = fin; 
                rsv1 = rsv1; 
                rsv2 = rsv2; 
                rsv3 = rsv3; 
                opCode = opCode; 
                masked = masked;
                payLoadLength = payLoadLength;
                payloadLengthExtended = payLoadLengthExtended; 
                payloadLengthActual = payLoadLengthActual;
                maskingKey = mask; 
                payload = payloadEncoded }

        }


    let writeFrame (frame) =
        use ms = new MemoryStream()

        bitWriter ms {
            do! BitWriter.WriteBool(frame.fin) 
            do! BitWriter.WriteBool(frame.rsv1)  
            do! BitWriter.WriteBool(frame.rsv2)  
            do! BitWriter.WriteBool(frame.rsv3)  
            do! BitWriter.WriteInt32(frame.opCode, 4)
            do! BitWriter.WriteBool(frame.masked)
            do! BitWriter.WriteByte(frame.payLoadLength, 7)            
            do! BitWriter.WriteBytes(frame.payloadLengthExtended)
            do! BitWriter.WriteBytes(frame.payload)
        }

        ms.ToArray()

    let writeFrameNew (frame) =
        use ms = new MemoryStream()

        let setBit (pos, bit) b: byte =
            if bit then
                (b ||| (1uy <<< pos))
            else
                b
            

        let byte1 = 
            frame.opCode
            |> byte            
            |> setBit (7, frame.fin)
            |> setBit (6, frame.rsv1)
            |> setBit (5, frame.rsv2)
            |> setBit (4, frame.rsv3)
            
        let byte2 =
            frame.payLoadLength            
            |> setBit (7, frame.masked)            
            
        ms.WriteByte(byte1)
        ms.WriteByte(byte2)
        
        frame.payloadLengthExtended |> Array.iter ms.WriteByte
        frame.payload |> Array.iter ms.WriteByte

        ms.ToArray()
       
    let makeFrame (data: byte array) = 

        let convertToNetworkByteOrder bytes = 
            bytes |> if BitConverter.IsLittleEndian then Array.rev else id
        
        let payloadLength = 
            match data.Length with
            | l when l < 126 -> (data.Length, [||])
            | l when l > 126 && l < 65536 -> (126, uint16 data.Length |> BitConverter.GetBytes)
            | _ -> (127, uint64 data.Length |> BitConverter.GetBytes)
        
        {
            fin = true;
            rsv1 = false;
            rsv2 = false;
            rsv3 = false;
            opCode = 1;
            masked = false;
            maskingKey = [||];
            payLoadLength = fst payloadLength |> uint8
            payloadLengthExtended = snd payloadLength |> convertToNetworkByteOrder
            payloadLengthActual = data.Length |> uint64
            payload = data            
        }
