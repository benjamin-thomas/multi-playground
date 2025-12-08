module Main0 exposing (main)

{-| REIFIED EFFECTS WITH CALLBACKS

This demonstrates the callback technique where effects contain functions.
Flow stays together in one Msg branch, but callbacks can't be fully tested.


## How it works

Effects are data structures that describe what should happen. The interpreter
converts these effects into Cmd Msg that actually perform the work.


## The Continue/TriggerMsg dance

Callbacks enable sequential async flows while keeping the code together:

    RegisterClicked
        │
        ↓
    update: returns Effect (SaveUser callback)
        │
        ↓
    interpret: generates random userId
        │
        ↓
    callback: receives userId, returns Effect
        │                                  ↓
        │                              TriggerMsg (UserSaved user userId)
        │                                  │
        │                                  ↓
        │                              interpret: sends msg to update
        │                                  │
        │                                  ↓
        │                              update: inserts user in Dict
        │                                  │
        │                                  ↓
        │                              returns None
        ↓
    Continue (batch of effects from callback)
        │
        ↓
    update: just passes the effect through
        │
        ↓
    interpret: processes Log, SendEmail, etc.

Key insight: TriggerMsg breaks out of the Effect chain to update the model,
while Continue keeps the Effect chain going for the rest of the effects.

-}

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, h2, input, p, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Process
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
-- EFFECTS AS DATA WITH CALLBACKS
-- Callbacks keep the flow together in one action!
--------------------------------------------------------------------------------


type Effect
    = Log String
    | SaveUser User (Int -> Effect) -- Callback receives UserId and returns next Effect!
    | SendEmail String String
    | ShowNotification String
    | Navigate String
    | TriggerMsg Msg -- Trigger a message to be processed through update!
    | None
    | Batch (List Effect)


{-| Manual equality - we can't compare callbacks
-}
effectsEqual : Effect -> Effect -> Bool
effectsEqual e1 e2 =
    case ( e1, e2 ) of
        ( Log msg1, Log msg2 ) ->
            msg1 == msg2

        ( SendEmail r1 m1, SendEmail r2 m2 ) ->
            r1 == r2 && m1 == m2

        ( ShowNotification n1, ShowNotification n2 ) ->
            n1 == n2

        ( Navigate url1, Navigate url2 ) ->
            url1 == url2

        ( None, None ) ->
            True

        ( Batch effects1, Batch effects2 ) ->
            List.length effects1 == List.length effects2 && List.all identity (List.map2 effectsEqual effects1 effects2)

        -- SaveUser can't be compared due to callback
        _ ->
            False



--------------------------------------------------------------------------------
-- ACTIONS
--------------------------------------------------------------------------------


type Msg
    = UsernameChanged String
    | EmailChanged String
    | RegisterClicked
    | UserSaved User Int -- When SaveUser completes, dispatch this with User and UserId!
    | LoginClicked
    | LogoutClicked
    | Continue Effect -- Continuation: handle the next effect from a callback
    | AddLog String



--------------------------------------------------------------------------------
-- UPDATE - Returns effects with callbacks!
-- Flow stays together in one action thanks to callbacks
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
                , SaveUser user
                    (\userId ->
                        -- Callback keeps the flow together!
                        -- We trigger UserSaved to update the model
                        Batch
                            [ TriggerMsg (UserSaved user userId)
                            , Log ("User saved with ID: " ++ String.fromInt userId)
                            , SendEmail
                                model.formEmail
                                ("Welcome! Your user ID is: " ++ String.fromInt userId)
                            , ShowNotification "Account created!"
                            , Navigate "/dashboard"
                            ]
                    )
                ]
            )

        UserSaved user userId ->
            -- This runs AFTER save completes (triggered by callback via interpreter!)
            -- We have all the data we need - no need to check currentUser!
            ( { model | users = Dict.insert userId user model.users }
            , None
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

        Continue effect ->
            -- Continuation: handle the next effect from a callback
            ( model, effect )

        AddLog logMsg ->
            ( { model | logs = logMsg :: model.logs }, None )



--------------------------------------------------------------------------------
-- INTERPRETER - Pattern match and execute effects
-- This would normally be in a separate module with Task/Cmd integration
--------------------------------------------------------------------------------


formatLog : String -> String -> String
formatLog label msg =
    String.padRight 10 ' ' ("[" ++ label ++ "]") ++ " | " ++ msg


interpret : Effect -> Cmd Msg
interpret effect =
    case effect of
        Log msg ->
            logCmd (formatLog "LOG" msg)

        SaveUser user callback ->
            -- Simulate async save with random ID generation
            -- The callback returns an Effect that we wrap in Continue
            Cmd.batch
                [ logCmd (formatLog "DB" ("Starting save for user: " ++ user.username))
                , Random.generate
                    (\userId ->
                        -- Callback returns an Effect, wrap it in Continue to interpret it
                        Continue (callback userId)
                    )
                    (Random.int 1000 9999)
                ]

        SendEmail recipient msg ->
            logCmd (formatLog "EMAIL" ("To: " ++ recipient ++ " | Message: " ++ msg))

        ShowNotification msg ->
            logCmd (formatLog "UI" msg)

        Navigate url ->
            logCmd (formatLog "NAV" url)

        TriggerMsg msg ->
            Task.perform identity (Task.succeed msg)

        None ->
            Cmd.none

        Batch effects ->
            -- NOTE: currently, the effects aren't really sequential
            -- Maybe I could "pluck out" one effect at a time to effectively schedule them sequentially? (I think)
            -- +> Batch [one, two] -> interpret one
            -- ++> Batch [two] -> interpret two
            -- +++> Batch [] -> NOOP
            Cmd.batch (List.map interpret effects)


logCmd : String -> Cmd Msg
logCmd msg =
    -- Simulate logging by adding to model
    -- In real app this would be Debug.log or a port
    Task.succeed msg
        |> Task.perform AddLog



--------------------------------------------------------------------------------
-- VIEW
--------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div []
        [ div [] [ Html.a [ Html.Attributes.href "/" ] [ text "← Back to index" ] ]
        , h1 [] [ text "Elm Reified Effects - Callbacks Version (Main0)" ]
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
        , p [] [ text "Uses callbacks in effects. Flow stays together but callbacks can't be tested/compared." ]
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
