type validated
type unverified
type reason = Insufficient_length
type 'validation password = Password of string

let from_string (str : string) : unverified password = Password str

let validate (Password pw : unverified password) : (validated password, reason) result =
  if String.length pw > 0 then
    Ok (Password pw)
  else
    Error Insufficient_length
;;

let string_of_reason = function
  | Insufficient_length -> "Not long enough"
;;
