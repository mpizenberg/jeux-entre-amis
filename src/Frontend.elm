module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Element)
import Element.Font
import Element.Input
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Lamdera exposing (ClientId)
import Set exposing (Set)
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , myName = ""
      , myId = ""
      , lobby = ""
      , existingLobbies = Set.fromList []
      , state = Types.Home
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case ( msg, model.state ) of
        ( UrlClicked urlRequest, _ ) ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        ( UrlChanged url, _ ) ->
            ( model, Cmd.none )

        ( NoOpFrontendMsg, _ ) ->
            ( model, Cmd.none )

        ( NameChange name, Home ) ->
            ( { model | myName = name }, Cmd.none )

        ( SelectInputMsg selectInput, Home ) ->
            ( { model | lobby = selectInput }, Cmd.none )

        ( ClickJoinLobbyBtn lobby, Home ) ->
            ( { model | state = Joining }
            , Lamdera.sendToBackend (JoinLobby { lobby = lobby, playerName = model.myName })
            )

        ( ClickStartGameBtn, Joined _ ) ->
            ( model, Lamdera.sendToBackend (StartGame { lobby = model.lobby }) )

        _ ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        Reset ->
            ( { model | state = Home }, Cmd.none )

        TellClientId id ->
            ( { model | myId = id }, Cmd.none )

        ListLobbies existingLobbies ->
            ( { model | existingLobbies = Set.fromList existingLobbies }, Cmd.none )

        LobbyCreated lobby ->
            ( { model | existingLobbies = Set.insert lobby model.existingLobbies }
            , Cmd.none
            )

        SomeoneJoined allPlayers ->
            ( { model | state = joinedState allPlayers }
            , Cmd.none
            )

        PickedRoles gameState ->
            case model.state of
                Joined { allPlayers } ->
                    ( { model | state = Joined { gameState = gameState, allPlayers = allPlayers } }
                    , Cmd.none
                    )

                _ ->
                    -- Should never happen
                    ( model, Cmd.none )


joinedState : List String -> FrontendState
joinedState allPlayers =
    Joined { gameState = WaitingForPlayersToConnect, allPlayers = allPlayers }


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Jeux entre amis : Espions"
    , body = [ Element.layout [] (elmUiView model) ]
    }


elmUiView : Model -> Element FrontendMsg
elmUiView model =
    case model.state of
        Home ->
            viewHome model

        Joining ->
            viewJoining model

        Joined { gameState, allPlayers } ->
            viewJoined model gameState allPlayers


viewHome : Model -> Element FrontendMsg
viewHome model =
    Element.column [ Element.centerX, Element.centerY, Element.spacing 24 ]
        [ viewNameInput model.myName
        , Element.html (viewSelect model.lobby (Set.toList model.existingLobbies))
            |> Element.el []
        , Element.row [ Element.spacing 20 ]
            [ createButton model
            , joinButton model
            ]
        ]


viewNameInput : String -> Element FrontendMsg
viewNameInput name =
    Element.Input.text []
        { onChange = NameChange
        , text = name
        , placeholder = Nothing
        , label = Element.Input.labelLeft [] (Element.text "Joueur :")
        }


createButton : { a | myName : String, lobby : String, existingLobbies : Set String } -> Element FrontendMsg
createButton { myName, lobby, existingLobbies } =
    if String.isEmpty myName || String.isEmpty lobby || Set.member lobby existingLobbies then
        Element.Input.button
            [ Element.htmlAttribute <| HA.style "cursor" "default"
            , Element.Font.color (Element.rgb255 130 130 130)
            ]
            { onPress = Nothing
            , label = Element.text ("Créer " ++ lobby)
            }

    else
        Element.Input.button []
            { onPress = Just (ClickJoinLobbyBtn lobby)
            , label = Element.text ("Créer " ++ lobby)
            }


joinButton : { a | myName : String, lobby : String, existingLobbies : Set String } -> Element FrontendMsg
joinButton { myName, lobby, existingLobbies } =
    if String.isEmpty myName || String.isEmpty lobby || not (Set.member lobby existingLobbies) then
        Element.Input.button
            [ Element.htmlAttribute <| HA.style "cursor" "default"
            , Element.Font.color (Element.rgb255 130 130 130)
            ]
            { onPress = Nothing
            , label = Element.text ("Rejoindre " ++ lobby)
            }

    else
        Element.Input.button []
            { onPress = Just (ClickJoinLobbyBtn lobby)
            , label = Element.text ("Rejoindre " ++ lobby)
            }


viewSelect : String -> List String -> Html FrontendMsg
viewSelect lobby existingLobbies =
    Html.div []
        [ Html.input
            [ HA.type_ "text"
            , HA.placeholder "Lobby"
            , HA.value lobby
            , HA.list "existingLobbies"
            , HE.onInput SelectInputMsg
            ]
            []
        , Html.datalist [ HA.id "existingLobbies" ]
            (List.map (\l -> Html.option [] [ Html.text l ]) existingLobbies)
        ]


viewJoining : Model -> Element FrontendMsg
viewJoining _ =
    Element.column [ Element.centerX, Element.centerY, Element.spacing 24 ]
        [ Element.text "En train de rejoindre ..." ]


viewJoined : Model -> GameState -> List String -> Element FrontendMsg
viewJoined model gameState allPlayers =
    case gameState of
        WaitingForPlayersToConnect ->
            viewWaitingForPlayers model.lobby allPlayers

        PickedTheSpiesAndWord spies word ->
            viewSpiesAndWord model.myId spies word


viewWaitingForPlayers : String -> List String -> Element FrontendMsg
viewWaitingForPlayers lobby allPlayers =
    Element.column [ Element.centerX, Element.centerY, Element.spacing 24 ]
        [ Element.text ("Participants du lobby " ++ lobby)
        , Element.column []
            (List.map Element.text allPlayers)
        , Element.Input.button []
            { onPress = Just ClickStartGameBtn
            , label = Element.text "Démarrer la partie!"
            }
        ]


viewSpiesAndWord : ClientId -> Set ClientId -> String -> Element FrontendMsg
viewSpiesAndWord myId spies word =
    let
        myText =
            if Set.member myId spies then
                "Mon mot : " ++ word

            else
                "Je n'ai pas de mot"
    in
    Element.column [ Element.centerX, Element.centerY, Element.spacing 24 ]
        [ Element.text myText
        ]



-- type alias FrontendModel =
--     { key : Key
--     , lobby : String
--     , existingLobbies : Set String
--     , myName : String
--     , state : FrontendState
--     , selectModel : SelectModel
--     }
--
-- type FrontendState
--     = Home
--     | Joining
--     | Joined { gameState : GameState, allPlayers : Set String }
--
-- type GameState
--     = WaitingForPlayersToConnect
--     | PickedTheSpiesAndWord (Set ClientId) String
