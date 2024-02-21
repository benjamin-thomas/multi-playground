-- runghc ./authentication.hs
-- echo ./authentication.hs | entr -c runghc /

import Text.Printf (printf)

data Validated
data Unverified
data Reason = InsufficientLength
newtype Password validation = Password String

fromString :: String -> Password Unverified
fromString = Password

validate :: Password Unverified -> Either Reason (Password Validated)
validate (Password pw)
    | length pw > 1 = Right (Password pw)
    | otherwise = Left InsufficientLength

stringOfReason :: Reason -> String
stringOfReason InsufficientLength = "Not long enough"

main :: IO ()
main = do
    let pass = fromString "p@a$$w0rd"
    case validate pass of
        Right (Password str) -> printf "Password check OK: %s!\n" str
        Left err -> printf "Password rejected: %s\n" (stringOfReason err)
