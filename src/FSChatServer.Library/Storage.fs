namespace FSChatServer.Library

open System.Collections.Generic
open FSChatServer.Library.Interface

module Storage =

    let inMemoryUserService (bullshitDatabase:List<User>) =        

        let getUser handle = bullshitDatabase |> Seq.tryFind (fun u -> u.handle = handle)               

        let getUserById userId = bullshitDatabase |> Seq.tryFind (fun u -> u.sessionId = userId)
    
        let addUser user = 
            bullshitDatabase.Add(user)
            Success user

        let removeUser user =            
            if bullshitDatabase.Remove(user) then
                Success(user)
            else
                Failed(sprintf "Failed to remove user %A" user)




        let getUserList () = List.ofSeq bullshitDatabase

        {
            getUser = getUser
            getUserById = getUserById
            addUser = addUser
            getUserList = getUserList
            removeUser = removeUser
        }



