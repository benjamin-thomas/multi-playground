{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

import Data.Function ((&))

-- Define phantom types to represent the state of each field
data Empty
data SelectSet
data FromSet
data WhereSet
data LimitSet

data Query = Query
    { select :: String
    , from :: String
    , where_ :: String
    , limit :: Int
    }
    deriving (Show)

-- Define a type to represent partial queries with phantom types
newtype PartialQuery (s :: *) (f :: *) (w :: *) (l :: *) = Partial Query
    deriving (Show)

-- Define a type to represent a full query
newtype FullQuery = Full Query
    deriving (Show)

emptyQuery :: PartialQuery Empty Empty Empty Empty
emptyQuery = Partial (Query "" "" "" 0)

-- Setters update the query and change the phantom type to indicate the field is set
setSelect :: String -> PartialQuery s f w l -> PartialQuery SelectSet f w l
setSelect sel (Partial query) = Partial query{select = sel}

setFrom :: String -> PartialQuery s f w l -> PartialQuery s FromSet w l
setFrom frm (Partial query) = Partial query{from = frm}

setWhere :: String -> PartialQuery s f w l -> PartialQuery s f WhereSet l
setWhere whr (Partial query) = Partial query{where_ = whr}

setLimit :: Int -> PartialQuery s f w l -> PartialQuery s f w LimitSet
setLimit limit (Partial query) = Partial query{limit}

-- Convert a fully constructed partial query into a full query
buildQuery :: PartialQuery SelectSet FromSet WhereSet LimitSet -> FullQuery
buildQuery (Partial query) = Full query

-- Example usage
myQuery :: FullQuery
myQuery =
    emptyQuery
        & setWhere "some_value >= 3"
        & setSelect "*"
        & setFrom "bobby_table"
        & setLimit 10
        & buildQuery