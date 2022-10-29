module Tests

open System

(*
    hello
    you appear to have stumbled into this test area
    there was a Unit Test project but for some reason after upgrading to VS2017, 
    I could not get get it to work.
    
    So I converted what was there to just regular vals that can be invoked in F# interactive.   
*)

module OldProtocolTests = 

    open FSChatServer.Library.OldProtocol
    open FSChatServer.Library.Interface
    
    let ``Should Correctly Parse Data`` =
        let data = "THIS_GUY\tPrimoz"

        let (command, data) = parseData data
        
        "THIS_GUY" = command && "Primoz" = data        
    
    let ``Should Correctly Interpret Command`` =
        let data = "THIS_GUY\t"

        let res = getServerCommand data

        match res with
        | Success(command) -> command = ServerCommand.ThisGuy
        | _ -> false
    
    let ``Test All Old Protocol Commands`` =
                
        Success(Login("primoz")) = getServerCommand "LOGIN\tprimoz" 
        && Success(Logout) = getServerCommand "DISCONNECT\t"
        && Success(SendMessage("primoz", "this guy")) = getServerCommand "SEND\tprimoz\tthis guy"
        && Success(GetUserList) = getServerCommand "GET_USER_LIST\t"
    
    let ``Server Commands Require Authentication`` =
        requiresAuthentication ServerCommand.GetUserList = true &&
        requiresAuthentication ServerCommand.ThisGuy = false
    
    let ``THIS GUY!!!`` =
        //included for historical reference
        false
            
    let ``GetUserList Should Create User List`` =
        
        let users = [
            { sessionId = Guid.NewGuid(); handle = "primoz" }
            { sessionId = Guid.NewGuid(); handle = "BrendoTRON3000" }
            ]
        let responseText = getUserList users
        let expectedText = "H\tLOGIN_NAME\tNICK_NAME\r\nD\tprimoz\tprimoz\r\nD\tBrendoTRON3000\tBrendoTRON3000\r\nE"
        expectedText = responseText

module ServerTests =

    open FSChatServer.Library.Server
    open FSChatServer.Library.Interface
    open System.Collections.Generic
    open FSChatServer.Library.Storage
    open System
                  
    //let handle = handleCommand inMemoryUserService

    let ``Validate User should return OK`` =
        let getUser _ = None
        let sessionId = Guid.NewGuid()
        Success { handle = "test"; sessionId = sessionId } = (validateUser getUser sessionId  "test")

//        [<TestMethod>]
//        member this.``Login Called More Than Once Should Error``() =                      
//            //requires some sort of session management**
//            //is this actually needed? Fundamentally, what is different
//            //compared to logging in with the same user?

//            ** Update 2019: really, ya think? Session management kind of fundamental to chat servers!

//            Assert.Inconclusive()
        

    
    let ``Validate User Called With Existing UserID Should Error`` =            
        let sessionId = Guid.NewGuid()
        let user = { sessionId = sessionId; handle = "test" }
        let getUser _ = Some user
        let res = validateUser getUser sessionId "test"
        let expected = LoginErrorUserIdExists(user)
        expected = res
        
    
    let ``Validate User Called Without UserID Should Error`` =
        let getUser _ = None
        
        LoginErrorNoUserId = validateUser getUser (Guid.NewGuid()) "" 

    //``Login Called Once Should Return OK``() =
    //    let service = inMemoryUserService(List<User>())
                        
    //    let sendClient command = Success()

    //    let sessionId = Guid.NewGuid()

    //    let response = handleCommand sendClient service (sessionId, ServerCommand.Login "test")
    //    Assert.AreEqual(OK, response, sprintf "Error: %A" response)

    //    Assert.AreEqual( Some { sessionId = sessionId; handle = "test" }, service.getUser("test"))            

    //[<TestMethod>]
    //member this.``Login Called Once Should Return Failed``() =
    //    let service = inMemoryUserService(List<User>())

    //    let sendClient command = Success()

    //    let sessionId = Guid.NewGuid()

    //    let response = handleCommand sendClient service (sessionId, ServerCommand.Login "")
            
    //    //Assert.AreEqual(Error, response, sprintf "Error: %A" response)

    //    Assert.AreEqual(None, service.getUser("test"))

    //[<TestMethod>]
    //member this.``Login Called More Than Once Should Error``() =        
    //    //requires some sort of session management
    //    //is this actually needed? Fundamentally, what is different
    //    //compared to logging in with the same user?
    //    Assert.Inconclusive()        

    //[<TestMethod>]
    //member this.``LoginUser should Broadcast AddUser Client Command``() =
    //    let mutable called = false;
    //    let sessionId = Guid.NewGuid()
    //    let sendClient command =  
    //        called <- true;
    //        match command with
    //        | AddUser u -> Success()
    //        | _ -> failwith "unexpected command"
    //    let addUser (user:User) = Success(user)
    //    let res = loginUser sendClient addUser sessionId "test"
            
    //    Assert.IsTrue(called);

    //[<TestMethod>]
    //member this.``GetUserList Should Return Notice if no users``() =
    //    let getUsers () = []
    //    let res = getCurrentUsers getUsers
    //    match res with
    //    | Notice _ -> Assert.AreEqual(GetUserListNoUsers, res)
    //    | _ -> Assert.Fail("Expecting Notice")

    //[<TestMethod>]
    //member this.``GetUserList Should Return Users``() =
    //    let getUsers () = [ { sessionId = Guid.NewGuid(); handle = "test" }  ]
    //    let res = getCurrentUsers getUsers
    //    //Assert.AreEqual(Result, res);
    //    match res with
    //    | Data(data) -> Assert.AreEqual(getUsers(), (match data with | UserList u -> u | _ -> failwith "Expecting UserList"))
    //    | _ -> Assert.Fail("Expecting result")

    //[<TestMethod>]
    //member this.``SendMessage Test``() =
                        
    //    let mutable (c:ClientCommand option) = None
    //    let sendClient command =
    //        c <- Some command
    //        Failed("todo")
            
    //    let res = sendMessage sendClient ("test2", "Heyo")
            
    //    Assert.AreEqual(OK, res)

    //    match c with
    //    | Some command -> Assert.AreEqual(ReceivedMessage("testFrom", "Hey"), command)
    //    | None -> Assert.Fail("Expecting sendClient to be called")