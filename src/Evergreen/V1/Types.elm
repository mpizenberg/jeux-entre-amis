module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Lamdera
import Set
import Url


type GameState
    = WaitingForPlayersToConnect
    | PickedTheSpiesAndWord (Set.Set Lamdera.ClientId) String


type FrontendState
    = Home
    | Joining
    | Joined
        { gameState : GameState
        , allPlayers : List String
        }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , lobby : String
    , existingLobbies : Set.Set String
    , myName : String
    , myId : Lamdera.ClientId
    , state : FrontendState
    }


type alias Game =
    { players : Dict.Dict Lamdera.ClientId String
    , alreadyPickedWords : Set.Set String
    , state : GameState
    }


type alias BackendModel =
    { games : Dict.Dict String Game
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | NameChange String
    | SelectInputMsg String
    | ClickJoinLobbyBtn String
    | ClickStartGameBtn


type ToBackend
    = JoinLobby
        { lobby : String
        , playerName : String
        }
    | StartGame
        { lobby : String
        }


type BackendMsg
    = NewConnection Lamdera.ClientId
    | Disconnection Lamdera.ClientId
    | DistributedRoles
        { lobby : String
        }
        ( Set.Set Lamdera.ClientId, String )


type ToFrontend
    = Reset
    | TellClientId Lamdera.ClientId
    | ListLobbies (List String)
    | LobbyCreated String
    | SomeoneJoined (List String)
    | PickedRoles GameState
