module Woql.Query exposing
    ( Object(..)
    , Predicate(..)
    , Query(..)
    , Subject(..)
    , Variables
    , encode
    )

import Json.Encode as Encode
import Schema


type Query
    = Select Variables Query
    | And (List Query)
    | Or (List Query)
    | Triple Subject Predicate Object
    | Optional Query


type Subject
    = Subject Schema.Value


type Predicate
    = Predicate Schema.Value


type Object
    = Object Schema.Value


type alias Variables =
    List String


encode : Query -> Encode.Value
encode query =
    Encode.object []
