{-

   echo ./src/ValidateFloat.elm | entr -c elm make /_ --output=/dev/null

   Reference material: https://www.youtube.com/watch?v=2OiWs-h_M3A&t=572s

   https://mermaid.live/edit#pako:eNqdkMEKgzAMhl-l5DxfwMNg4FV2cMdego1asO1oU2So776KG5tsOFhO-cr3kyYj1E4R5ND0bqg79CwuhbQiVcULbSrLjpPRNoZJnBomXy79ar94Yyvdap5ERbWzqljgbAf0KuzO-JX6eP8_JcSkHD_2KRy_bZPo-79KtKxDwNV90r4LBzDkDWqVjj0uSQnckSEJeWoVNRh7liDtnFSM7KqbrSFnH-kA8aqQqdDYejSQN9gHmu_NU43H

   flowchart TD
    Start              -->|minus| AfterMinus
    AfterMinus         -->|digit| SecondDigitOnwards
    Start              -->|digit| SecondDigitOnwards
    SecondDigitOnwards -->|digit| SecondDigitOnwards
    SecondDigitOnwards -->  |dot| AfterDot
    AfterDot           -->|digit| Mantissa
    Mantissa           -->|digit| Mantissa

-}


module ValidateFloat exposing (State(..), init, run)

import Char exposing (isDigit)



-- UTILITY


isDigit : Char -> Bool
isDigit c =
    List.member c (String.toList "0123456789")


isDot : Char -> Bool
isDot c =
    c == '.'


isMinus : Char -> Bool
isMinus c =
    c == '-'



-- STATE MACHINE


type State
    = Start
    | AfterMinus
    | SecondDigitOnwards
    | AfterDot
    | Mantissa
    | Fail
    | Success


type Event
    = Minus
    | Digit
    | Dot
    | BadChar
    | Empty



-- MODEL


type alias Model =
    { state : State
    , input : List Char
    }


init : List Char -> Model
init chars =
    { state = Start, input = chars }



-- TRANSITION


ifHead : Model -> (Char -> Bool) -> State -> Model
ifHead model headIs nextState =
    case model.input of
        [] ->
            { model | state = Fail }

        x :: xs ->
            if headIs x then
                { model | input = xs, state = nextState }

            else
                { model | state = Fail }


transition : Event -> Model -> Model
transition event model =
    let
        ifHead_ =
            ifHead model
    in
    case ( model.state, event ) of
        ( Start, Minus ) ->
            ifHead_ isDigit AfterMinus

        ( Start, Digit ) ->
            ifHead_ isDigit SecondDigitOnwards

        ( AfterMinus, Digit ) ->
            ifHead_ isDigit SecondDigitOnwards

        ( SecondDigitOnwards, Digit ) ->
            ifHead_ isDigit SecondDigitOnwards

        ( SecondDigitOnwards, Dot ) ->
            ifHead_ isDot AfterDot

        ( AfterDot, Digit ) ->
            ifHead_ isDigit Mantissa

        ( Mantissa, Digit ) ->
            ifHead_ isDigit Mantissa

        ( Mantissa, Empty ) ->
            { model | state = Success }

        ( SecondDigitOnwards, Empty ) ->
            { model | state = Success }

        _ ->
            { model | state = Fail }


nextEvent : Maybe Char -> Event
nextEvent maybeChar =
    case maybeChar of
        Nothing ->
            Empty

        Just c ->
            if isDigit c then
                Digit

            else if isMinus c then
                Minus

            else if isDot c then
                Dot

            else
                BadChar


consume : Model -> Model
consume model =
    transition (nextEvent (List.head model.input)) model


run : Model -> Model
run model =
    case model.state of
        Fail ->
            model

        Success ->
            model

        _ ->
            run (consume model)
