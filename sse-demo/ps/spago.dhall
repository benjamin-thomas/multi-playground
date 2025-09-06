{ name = "sse-demo"
, dependencies =
  [ "aff"
  , "argonaut"
  , "arrays"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foreign"
  , "httpurple"
  , "maybe"
  , "node-buffer"
  , "node-http"
  , "node-process"
  , "node-streams"
  , "now"
  , "ordered-collections"
  , "posix-types"
  , "prelude"
  , "refs"
  , "routing-duplex"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
