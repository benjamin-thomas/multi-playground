{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Function ((&))
import GHC.TypeLits (Symbol)

data Query = Query
    { querySelect :: String
    , queryFrom :: String
    , queryWhere :: String
    , queryLimit :: Int
    }
    deriving (Show)

-- PartialQuery with type-level list to track fields
newtype PartialQuery (s :: [Symbol])
    = PartialQuery Query
    deriving (Show)

-- FullQuery when all fields are set
newtype FullQuery = FullQuery Query
    deriving (Show)

-- Empty query
emptyQuery :: PartialQuery '[]
emptyQuery =
    PartialQuery $
        Query
            { querySelect = ""
            , queryFrom = ""
            , queryWhere = ""
            , queryLimit = 0
            }

-- Type-level function to add a field to the list
type family AddField (field :: Symbol) (fields :: [Symbol]) :: [Symbol] where
    AddField field fields = field ': fields

-- Type class to ensure no duplicate fields
class UniqueField field fields

instance (Elem field fields ~ 'False) => UniqueField field fields

-- Type-level function to check if a field is in the list
type family Elem (field :: Symbol) (fields :: [Symbol]) :: Bool where
    Elem field '[] = 'False
    Elem field (field ': _) = 'True
    Elem field (_ ': rest) = Elem field rest

-- Setting fields
setSelect :: forall s. (UniqueField "Select" s) => String -> PartialQuery s -> PartialQuery (AddField "Select" s)
setSelect str (PartialQuery query) = PartialQuery $ query{querySelect = str}

setFrom :: forall s. (UniqueField "From" s) => String -> PartialQuery s -> PartialQuery (AddField "From" s)
setFrom from (PartialQuery query) = PartialQuery $ query{queryFrom = from}

setWhere :: forall s. (UniqueField "Where" s) => String -> PartialQuery s -> PartialQuery (AddField "Where" s)
setWhere str (PartialQuery query) = PartialQuery $ query{queryWhere = str}

setLimit :: forall s. (UniqueField "Limit" s) => Int -> PartialQuery s -> PartialQuery (AddField "Limit" s)
setLimit limit (PartialQuery query) = PartialQuery $ query{queryLimit = limit}

-- Type class to ensure all fields are set before converting to FullQuery
class BuildFullQuery s where
    buildFullQuery :: PartialQuery s -> FullQuery

-- Ensure all four fields are present in any order
instance
    ( Elem "Select" s ~ 'True
    , Elem "From" s ~ 'True
    , Elem "Where" s ~ 'True
    , Elem "Limit" s ~ 'True
    ) =>
    BuildFullQuery s
    where
    buildFullQuery (PartialQuery query) = FullQuery query

myQuery :: FullQuery
myQuery =
    emptyQuery
        & setSelect "*"
        & setFrom "bobby_table"
        & setWhere "some_value >= 3"
        & setLimit 10
        & buildFullQuery
