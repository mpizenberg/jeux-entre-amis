module Backend exposing (..)

import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Random
import Random.List
import Set
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { games = Dict.empty }
    , Cmd.none
    )


subscriptions : Model -> Sub BackendMsg
subscriptions _ =
    Sub.batch
        [ Lamdera.onConnect (\_ clientid -> NewConnection clientid)
        , Lamdera.onDisconnect (\_ clientid -> Disconnection clientid)
        ]


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NewConnection clientId ->
            let
                existingLobbies =
                    Dict.keys model.games
            in
            ( model
            , Cmd.batch
                [ Lamdera.sendToFrontend clientId (ListLobbies existingLobbies)
                , Lamdera.sendToFrontend clientId (TellClientId clientId)
                ]
            )

        Disconnection clientId ->
            let
                ( gamesWithClientId, gamesWithoutClientId ) =
                    Dict.partition (\_ game -> Dict.member clientId game.players) model.games

                -- Re-add updated games if possible
                ( updatedGames, commands ) =
                    Dict.foldl
                        (\lobby game otherGames -> helperDisconnection clientId lobby game otherGames)
                        ( gamesWithoutClientId, [] )
                        gamesWithClientId
            in
            ( { model | games = updatedGames }
            , Cmd.batch
                -- TODO improve by not broadcasting
                (Lamdera.broadcast (ListLobbies (Dict.keys updatedGames))
                    :: commands
                )
            )

        DistributedRoles { lobby } ( spies, word ) ->
            case Dict.get lobby model.games of
                Nothing ->
                    -- Should never happen
                    ( model, Cmd.none )

                Just game ->
                    -- TODO: update game state and send to frontend
                    let
                        newState =
                            PickedTheSpiesAndWord spies word

                        updatedGame =
                            { game | state = newState }
                    in
                    ( { model | games = Dict.insert lobby updatedGame model.games }
                    , sendToPlayers (PickedRoles newState) (Dict.keys game.players)
                    )


sendToPlayers : ToFrontend -> List ClientId -> Cmd BackendMsg
sendToPlayers msg players =
    List.map (\id -> Lamdera.sendToFrontend id msg) players
        |> Cmd.batch



--
-- type alias Game =
--     { players : Dict ClientId String
--     , alreadyPickedWords : Set String
--     , state : GameState
--     }
--
--
-- type GameState
--     = WaitingForPlayersToConnect
--     | PickedTheSpiesAndWord (Set ClientId) String


helperDisconnection : ClientId -> String -> Game -> ( Dict String Game, List (Cmd BackendMsg) ) -> ( Dict String Game, List (Cmd BackendMsg) )
helperDisconnection clientId lobby game ( updatedGames, commands ) =
    -- Currently, we just have one game that can only be updated if the game hasn't started yet
    case game.state of
        WaitingForPlayersToConnect ->
            let
                updatedPlayers =
                    Dict.remove clientId game.players

                playerNames =
                    Dict.values updatedPlayers

                newUpdatedGames =
                    if Dict.isEmpty updatedPlayers then
                        updatedGames

                    else
                        Dict.insert lobby { game | players = updatedPlayers } updatedGames
            in
            ( newUpdatedGames
              -- Update the list of players for all remaining players
            , (Dict.keys updatedPlayers
                |> List.map (\id -> Lamdera.sendToFrontend id (SomeoneJoined playerNames))
                |> Cmd.batch
              )
                :: commands
            )

        PickedTheSpiesAndWord _ _ ->
            ( updatedGames
              -- Reset all other players since we can't continue the game
            , (Dict.keys game.players
                |> List.map (\id -> Lamdera.sendToFrontend id Reset)
                |> Cmd.batch
              )
                :: commands
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        JoinLobby { lobby, playerName } ->
            case Dict.get lobby model.games of
                Nothing ->
                    let
                        newGame =
                            { players = Dict.fromList [ ( clientId, playerName ) ]
                            , alreadyPickedWords = Set.empty
                            , state = WaitingForPlayersToConnect
                            }
                    in
                    ( { model | games = Dict.insert lobby newGame model.games }
                    , Cmd.batch
                        [ Lamdera.broadcast (LobbyCreated lobby)
                        , Lamdera.sendToFrontend clientId (SomeoneJoined (Dict.values newGame.players))
                        ]
                    )

                Just game ->
                    let
                        newGame =
                            { game | players = Dict.insert clientId playerName game.players }
                    in
                    ( { model | games = Dict.insert lobby newGame model.games }
                    , List.map
                        (\id -> Lamdera.sendToFrontend id (SomeoneJoined (Dict.values newGame.players)))
                        (Dict.keys newGame.players)
                        |> Cmd.batch
                    )

        StartGame { lobby } ->
            case Dict.get lobby model.games of
                Nothing ->
                    -- Should never happen
                    ( model, Cmd.none )

                Just game ->
                    let
                        players =
                            Dict.keys game.players

                        pickTwoSpiesGenerator =
                            Random.List.shuffle players
                                |> Random.map (List.take 2 >> Set.fromList)

                        wordGenerator =
                            Random.uniform "Toto" [ "Tata", "Titi" ]

                        spiesAndWordGenerator =
                            Random.map2 Tuple.pair
                                pickTwoSpiesGenerator
                                wordGenerator
                    in
                    ( model, Random.generate (DistributedRoles { lobby = lobby }) spiesAndWordGenerator )



--
-- type alias BackendModel =
--     { games : Dict String Game }
--
--
-- type alias Game =
--     { players : Dict ClientId String
--     , alreadyPickedWords : Set String
--     , state : GameState
--     }
--
--
-- type GameState
--     = WaitingForPlayersToConnect
--     | PickedTheSpiesAndWord (Set ClientId) String
