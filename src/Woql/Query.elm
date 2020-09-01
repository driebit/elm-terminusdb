module Woql.Query exposing
    ( Object(..)
    , Predicate(..)
    , Query(..)
    , Subject(..)
    , Value(..)
    , Variables
    , encode
    )

import Json.Encode as Encode
import Schema
import Schema.Prefix as Prefix exposing (Prefix(..))
import Schema.Xsd as Xsd


type Query
    = Select Variables Query
    | And (List Query)
    | Or (List Query)
    | Triple Subject Predicate Object
    | Optional Query


type Subject
    = Subject Value


type Predicate
    = Predicate Value


type Object
    = Object Value


type Value
    = Var String
    | Node String


type alias Variables =
    List String


encode : List Prefix -> Query -> Encode.Value
encode prefixes query =
    let
        -- this handling of contexts is a blunt workaround, but works for now
        context =
            Prefix.context <|
                [ Woql, Xsd ]
                    ++ prefixes
    in
    encodeSubQuery (Just context) query


encodeSubQuery : Maybe Prefix.Context -> Query -> Encode.Value
encodeSubQuery context query =
    Encode.object <|
        (case query of
            Select vars subQuery ->
                [ ( "@type", Encode.string "woql:Select" )
                , ( "woql:variable_list", encodeVariableList vars )
                , ( "woql:query", encodeSubQuery Nothing subQuery )
                ]

            And queries ->
                [ ( "@type", Encode.string "woql:And" )
                , ( "woql:query_list", encodeQueryList queries )
                ]

            Or queries ->
                [ ( "@type", Encode.string "woql:Or" )
                , ( "woql:query_list", encodeQueryList queries )
                ]

            Triple (Subject subject) (Predicate predicate) (Object object) ->
                [ ( "@type", Encode.string "woql:Triple" )
                , ( "woql:subject", encodeValue subject )
                , ( "woql:predicate", encodeValue predicate )
                , ( "woql:object", encodeValue object )
                ]

            Optional subquery ->
                [ ( "@type", Encode.string "woql:Optional" )
                , ( "woql:query", encodeSubQuery Nothing subquery )
                ]
        )
            ++ (case context of
                    Nothing ->
                        []

                    Just c ->
                        [ ( "@context", Prefix.encodeContext c ) ]
               )


encodeVariableList : List String -> Encode.Value
encodeVariableList variables =
    Encode.list
        (encodeIndexedListItem "woql:VariableListItem" "woql:variable_name" Xsd.encodeString)
        (List.indexedMap Tuple.pair variables)


encodeIndexedListItem : String -> String -> (a -> Encode.Value) -> ( Int, a ) -> Encode.Value
encodeIndexedListItem typeName valueName encoder ( index, value ) =
    Encode.object
        [ ( "@type", Encode.string typeName )
        , ( valueName, encoder value )
        , ( "woql:index", Xsd.encodeNonNegativeInt index )
        ]


encodeQueryList : List Query -> Encode.Value
encodeQueryList queries =
    Encode.list
        (encodeIndexedListItem "woql:QueryListElement" "woql:query" (encodeSubQuery Nothing))
        (List.indexedMap Tuple.pair queries)


encodeValue : Value -> Encode.Value
encodeValue value =
    case value of
        Node node ->
            Encode.object
                [ ( "@type", Encode.string "woql:Node" )
                , ( "woql:node", Encode.string node )
                ]

        Var name ->
            Encode.object
                [ ( "@type", Encode.string "woql:Variable" )
                , ( "woql:variable_name", Xsd.encodeString name )
                ]
