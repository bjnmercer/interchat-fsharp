namespace FSChatServer.Library

open Interface

module Session =

    type SessionInfo = {
        id: string
        user: User
    }

    let getSession id =
        ()

