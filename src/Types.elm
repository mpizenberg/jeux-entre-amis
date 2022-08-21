module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId)
import Set exposing (Set)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , lobby : String
    , existingLobbies : Set String
    , myName : String
    , myId : ClientId
    , state : FrontendState
    }


type FrontendState
    = Home
    | Joining
    | Joined { gameState : GameState, allPlayers : List String }


type alias BackendModel =
    { games : Dict String Game }


type alias Game =
    { players : Dict ClientId String
    , alreadyPickedWords : Set String
    , state : GameState
    }


type GameState
    = WaitingForPlayersToConnect
    | PickedTheSpiesAndWord (Set ClientId) String


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | NameChange String
    | SelectInputMsg String
    | ClickJoinLobbyBtn String
    | ClickStartGameBtn


type ToBackend
    = JoinLobby { lobby : String, playerName : String }
    | StartGame { lobby : String }


type BackendMsg
    = NewConnection ClientId
    | Disconnection ClientId
    | DistributedRoles { lobby : String } ( Set ClientId, String )


type ToFrontend
    = Reset
    | TellClientId ClientId
    | ListLobbies (List String)
    | LobbyCreated String
    | SomeoneJoined (List String)
    | PickedRoles GameState
