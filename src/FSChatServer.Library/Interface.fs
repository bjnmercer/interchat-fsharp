namespace FSChatServer.Library

open System

module Interface =
    //open System.Net.Sockets

    type ServerCommand =
    | ThisGuy   
    | Login of userId:string
    | Logout
    | SendMessage of userId:string * message:string
    | GetUserList

    let requiresAuthentication (command:ServerCommand) =
        match command with
        | ThisGuy | Login(_) | (Logout) -> false
        | _ -> true
    
    type User = {
        sessionId: Guid
        handle: string
    }

    type Result<'a> =
        | Success of 'a
        | Failed of message:string

    let bind switchFunction twoTrackInput = 
        match twoTrackInput with
        | Success s -> switchFunction s
        | Failed f -> Failed f

    let (>>=) twoTrackInput switchFunction = 
        bind switchFunction twoTrackInput

    type GetUser = string -> User option
    type GetUserById = Guid -> User option
    type AddUser = User -> Result<User>
    type RemoveUser = User -> Result<User>
    type GetUserList = unit -> User list

    type CommandHandlerFunctions = {
        getUser: GetUser
        getUserById: GetUserById
        addUser: AddUser
        removeUser: RemoveUser
        getUserList: GetUserList
    }

    type ServerDataResult =    
    | ThisGuy
    | UserList of User list

//    type ServerResponse =
//    //| ThisGuy
//    | OK
//    | Data of data:ServerDataResult
//    | Error of message:string
//    | Notice of message:string
//    | Unauthenticated
    //| UserList of User list    

    type ReceivedMessage = {
        fromUserId:string
        toUserId:string
        message:string
    }

    type ClientCommand =
    | OK    
    | ReceivedMessage of ReceivedMessage//fromUserId:string * toUserId:string * message:string
    | Data of data:ServerDataResult
    | Error of message:string
    | Notice of message:string
    | Unauthenticated
    | AddUser of user:User
    | DeleteUser of userId:string
    | Stop


    type SessionCommand =
    | AddSession of sessionId:Guid * mailbox:MailboxProcessor<ClientCommand>
    //| LoginUser of sessionId:Guid * userId:string
    | RemoveSession of sessionId:Guid
    | SendCommand of sessionId:Guid * command:Result<ServerCommand>    
    //| SendClient of sessionId:Guid * command:ClientCommand
    //| SendBroadcast of sessionId:Guid * command:ClientCommand
    //| SendServerResponse of sessionId:Guid * response:ServerResponse


    type GetServerCommand = string -> ServerCommand option

    type CommandHandler = ServerCommand -> ClientCommand

    //type SendClientCommand = ClientCommand -> Result<unit>

