-- echo ./src/Authentication.elm | entr -c elm make /_ --output /dev/null


module Authentication exposing (..)


type Valid
    = Valid


type Unverified
    = Unverified


type Password validation
    = Password String


fromString : String -> Password Unverified
fromString str =
    Password str


validate : Password Unverified -> Result String (Password Valid)
validate (Password pw) =
    if String.length pw > 0 then
        Ok (Password pw)

    else
        Err "wat"


main_ : ()
main_ =
    let
        pass : Password Unverified
        pass =
            fromString "Hello"

        validated : Result String (Password Valid)
        validated =
            validate pass
    in
    case validated of
        Ok (Password pw) ->
            ()

        Err str ->
            ()
