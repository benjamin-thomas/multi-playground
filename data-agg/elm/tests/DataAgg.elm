module DataAgg exposing (tests)

import Dict exposing (Dict)
import Expect
import List
import Test exposing (Test, describe, test)


type alias Author =
    { id : Int
    , firstName : String
    , lastName : String
    }


type alias Row =
    { authorId : Int
    , authorFirstName : String
    , authorLastName : String
    , bookId : Int
    , bookTitle : String
    , isbn : String
    }


type alias Book =
    { id : Int
    , title : String
    , isbn : String
    }


type alias AuthorView =
    { author : Author
    , books : List Book
    }


toTree : List Row -> List AuthorView
toTree rows =
    let
        collapse : Row -> Maybe AuthorView -> Maybe AuthorView
        collapse row authorView =
            case authorView of
                Nothing ->
                    let
                        author : Author
                        author =
                            { id = row.authorId
                            , firstName = row.authorFirstName
                            , lastName = row.authorLastName
                            }

                        book : Book
                        book =
                            { id = row.bookId
                            , title = row.bookTitle
                            , isbn = row.isbn
                            }
                    in
                    Just
                        { author = author
                        , books = [ book ]
                        }

                Just curr ->
                    let
                        book : Book
                        book =
                            { id = row.bookId
                            , title = row.bookTitle
                            , isbn = row.isbn
                            }
                    in
                    Just
                        { curr | books = book :: curr.books }

        step : Row -> Dict Int AuthorView -> Dict Int AuthorView
        step row acc =
            Dict.update
                row.authorId
                (collapse row)
                acc

        collapseAll : List Row -> Dict Int AuthorView
        collapseAll =
            List.foldl step Dict.empty

        sortBooks authorView =
            { authorView
                | books = List.sortBy .id authorView.books
            }
    in
    rows
        |> collapseAll
        |> Dict.toList
        |> List.map Tuple.second
        |> List.map sortBooks


testAuthorView : Test
testAuthorView =
    let
        dbRows : List Row
        dbRows =
            [ { authorId = 111
              , authorFirstName = "John"
              , authorLastName = "Whitington"
              , bookId = 111
              , bookTitle = "OCaml from the Very Beginning"
              , isbn = "111-111"
              }
            , { authorId = 111
              , authorFirstName = "John"
              , authorLastName = "Whitington"
              , bookId = 222
              , bookTitle = "More OCaml"
              , isbn = "111-222"
              }
            , { authorId = 222
              , authorFirstName = "Graham"
              , authorLastName = "Hutton"
              , bookId = 333
              , bookTitle = "Programming in Haskell"
              , isbn = "222-333"
              }
            , { authorId = 333
              , authorFirstName = "Yaron"
              , authorLastName = "Minsky"
              , bookId = 444
              , bookTitle = "Real World OCaml"
              , isbn = "333-444+555"
              }
            , { authorId = 444
              , authorFirstName = "Anil"
              , authorLastName = "Madhavapeddy"
              , bookId = 444
              , bookTitle = "Real World OCaml"
              , isbn = "333-444+555"
              }
            ]
    in
    test "Author_view is equal" <|
        \() ->
            let
                want =
                    [ { author = { id = 111, firstName = "John", lastName = "Whitington" }
                      , books =
                            [ { id = 111, title = "OCaml from the Very Beginning", isbn = "111-111" }
                            , { id = 222, title = "More OCaml", isbn = "111-222" }
                            ]
                      }
                    , { author = { id = 222, firstName = "Graham", lastName = "Hutton" }
                      , books = [ { id = 333, title = "Programming in Haskell", isbn = "222-333" } ]
                      }
                    , { author = { id = 333, firstName = "Yaron", lastName = "Minsky" }
                      , books = [ { id = 444, title = "Real World OCaml", isbn = "333-444+555" } ]
                      }
                    , { author = { id = 444, firstName = "Anil", lastName = "Madhavapeddy" }
                      , books = [ { id = 444, title = "Real World OCaml", isbn = "333-444+555" } ]
                      }
                    ]

                got =
                    toTree dbRows
            in
            Expect.equal want got


tests : Test
tests =
    describe "Author view"
        [ testAuthorView
        ]
