type validated
type unverified
type reason = Insufficient_length
type 'validation password = Password of string

val from_string : string -> unverified password
val validate : unverified password -> (validated password, reason) result
val string_of_reason : reason -> string
