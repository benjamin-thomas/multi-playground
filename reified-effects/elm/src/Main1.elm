module Main1 exposing (main)

{-| REIFIED EFFECTS WITH SCATTERED ACTIONS
This is the standard Elm approach - no callbacks, just pure data.
Flow is scattered across multiple Msg branches, but fully testable!
-}

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, h2, input, p, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Random
import Task


--------------------------------------------------------------------------------
-- DOMAIN: A simple user registration system
--------------------------------------------------------------------------------


type alias Model =
    { users : Dict Int User
    , currentUser : Maybe User
    , formUsername : String
    , formEmail : String
    , logs : List String
    }


type alias User =
    { username : String
    , email : String
    }


initialModel : Model
initialModel =
    { users = Dict.empty
    , currentUser = Nothing
    , formUsername = "John"
    , formEmail = "john@example.com"
    , logs = []
    }


--------------------------------------------------------------------------------
-- EFFECTS AS DATA (reified!)
-- These are just descriptions of what we want to happen - NO CALLBACKS!
--------------------------------------------------------------------------------


type Effect
    = Log String
    | SaveUser User -- No callback! Interpreter will dispatch UserSaved when done
    | SendEmail String String
    | ShowNotification String
    | Navigate String
    | None
    | Batch (List Effect)


{-| Effects can be compared for equality - they're pure data!
-}
effectsEqual : Effect -> Effect -> Bool
effectsEqual e1 e2 =
    e1 == e2  -- Can derive Eq since no functions!


--------------------------------------------------------------------------------
-- ACTIONS (like Elm messages)
--------------------------------------------------------------------------------


type Msg
    = UsernameChanged String
    | EmailChanged String
    | RegisterClicked
    | UserSaved User Int -- Async callback when save completes with User and UserId!
    | LoginClicked
    | LogoutClicked
    | AddLog String


--------------------------------------------------------------------------------
-- UPDATE - PURE! Returns new model + effects as data
-- Flow is scattered across multiple Msg branches
--------------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        UsernameChanged username ->
            ( { model | formUsername = username }, None )

        EmailChanged email ->
            ( { model | formEmail = email }, None )

        RegisterClicked ->
            let
                user =
                    { username = model.formUsername
                    , email = model.formEmail
                    }
            in
            ( { model | currentUser = Just user }
            , Batch
                [ Log ("Registering user: " ++ model.formUsername)
                , ShowNotification "Creating account..."
                , SaveUser user -- When done, interpreter will dispatch UserSaved
                ]
            )

        UserSaved user userId ->
            -- This runs AFTER save completes (scattered from RegisterClicked!)
            -- We have all the data we need - no need to check currentUser!
            ( { model | users = Dict.insert userId user model.users }
            , Batch
                [ Log ("User saved with ID: " ++ String.fromInt userId)
                , SendEmail
                    user.email
                    ("Welcome! Your user ID is: " ++ String.fromInt userId)
                , ShowNotification "Account created!"
                , Navigate "/dashboard"
                ]
            )

        LoginClicked ->
            let
                maybeUser =
                    Dict.values model.users
                        |> List.filter (\u -> u.username == model.formUsername)
                        |> List.head
            in
            case maybeUser of
                Nothing ->
                    ( model, ShowNotification "User not found" )

                Just user ->
                    ( { model | currentUser = Just user }
                    , Batch
                        [ Log ("User logged in: " ++ model.formUsername)
                        , ShowNotification ("Welcome back, " ++ model.formUsername ++ "!")
                        , Navigate "/dashboard"
                        ]
                    )

        LogoutClicked ->
            ( { model | currentUser = Nothing }
            , Batch
                [ Log "User logged out"
                , ShowNotification "Goodbye!"
                , Navigate "/login"
                ]
            )

        AddLog logMsg ->
            ( { model | logs = logMsg :: model.logs }, None )


--------------------------------------------------------------------------------
-- INTERPRETER - The ONLY place with Cmd!
-- Converts our effect data into actual Cmd operations
--
-- The interpreter closes the loop for async operations:
--   Msg → update → Effect → interpret → Msg → update → ...
--
-- When async effects complete, they dispatch new messages back to update
--------------------------------------------------------------------------------


formatLog : String -> String -> String
formatLog label msg =
    String.padRight 10 ' ' ("[" ++ label ++ "]") ++ " | " ++ msg


interpret : Effect -> Cmd Msg
interpret effect =
    case effect of
        Log msg ->
            logCmd (formatLog "LOG" msg)

        SaveUser user ->
            Cmd.batch
                [ logCmd (formatLog "DB" ("Starting async save for user: " ++ user.username))
                , Random.generate (UserSaved user) (Random.int 1000 9999)
                ]

        SendEmail recipient msg ->
            logCmd (formatLog "EMAIL" ("To: " ++ recipient ++ " | Message: " ++ msg))

        ShowNotification msg ->
            logCmd (formatLog "UI" msg)

        Navigate url ->
            logCmd (formatLog "NAV" url)

        None ->
            Cmd.none

        Batch effects ->
            Cmd.batch (List.map interpret effects)


logCmd : String -> Cmd Msg
logCmd msg =
    Task.succeed msg
        |> Task.perform AddLog


--------------------------------------------------------------------------------
-- VIEW
--------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div []
        [ div [] [ Html.a [ Html.Attributes.href "/" ] [ text "← Back to index" ] ]
        , h1 [] [ text "Elm Reified Effects - Scattered Actions Version (Main1)" ]
        , h2 [] [ text "User Registration Demo" ]
        , div []
            [ input [ placeholder "Username", value model.formUsername, onInput UsernameChanged ] []
            , input [ placeholder "Email", value model.formEmail, onInput EmailChanged ] []
            ]
        , div []
            [ button [ onClick RegisterClicked ] [ text "Register" ]
            , button [ onClick LoginClicked ] [ text "Login" ]
            , button [ onClick LogoutClicked ] [ text "Logout" ]
            ]
        , h2 [] [ text "Current State" ]
        , case model.currentUser of
            Nothing ->
                p [] [ text "No user logged in" ]

            Just user ->
                p [] [ text ("Logged in as: " ++ user.username ++ " (" ++ user.email ++ ")") ]
        , div []
            [ h2 [] [ text "Registered Users" ]
            , div [] (Dict.values model.users |> List.map (\u -> p [] [ text u.username ]))
            ]
        , h2 [] [ text "Logs (most recent last)" ]
        , Html.pre [ Html.Attributes.style "font-family" "monospace" ] [ text (String.join "\n" (List.reverse model.logs)) ]
        , h2 [] [ text "About This Version" ]
        , p [] [ text "Scattered actions approach (standard Elm). Flow is split but fully testable!" ]
        , p [] [ text "Effects are pure data with no callbacks - can be compared with ==." ]
        ]


--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, None |> interpret )
        , view = view
        , update = \msg model -> update msg model |> Tuple.mapSecond interpret
        , subscriptions = \_ -> Sub.none
        }
