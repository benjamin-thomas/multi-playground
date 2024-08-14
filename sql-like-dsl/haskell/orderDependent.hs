import Data.Function ((&))

data Query = Query
    { querySelect :: String
    , queryFrom :: String
    , queryWhere :: String
    , queryLimit :: Int
    }
    deriving (Show)

data Empty
data WithSelect
data WithFrom
data WithWhere

newtype PartialQuery a
    = PartialQuery Query
    deriving (Show)

newtype FullQuery
    = FullQuery Query
    deriving (Show)

emptyQuery :: PartialQuery Empty
emptyQuery =
    PartialQuery $
        Query
            { querySelect = ""
            , queryFrom = ""
            , queryWhere = ""
            , queryLimit = 0
            }

setSelect :: String -> PartialQuery Empty -> PartialQuery WithSelect
setSelect str (PartialQuery query) = PartialQuery $ query{querySelect = str}

setFrom :: String -> PartialQuery WithSelect -> PartialQuery WithFrom
setFrom from (PartialQuery query) = PartialQuery $ query{queryFrom = from}

setWhere :: String -> PartialQuery WithFrom -> PartialQuery WithWhere
setWhere str (PartialQuery query) =
    PartialQuery $
        query{queryWhere = str}

setLimit :: Int -> PartialQuery WithWhere -> FullQuery
setLimit limit (PartialQuery query) =
    FullQuery $
        query{queryLimit = limit}

myQuery :: FullQuery
myQuery =
    emptyQuery
        & setSelect "*"
        & setFrom "bobby_table"
        & setWhere "some_value >= 3"
        & setLimit 10
