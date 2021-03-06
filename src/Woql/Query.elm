module Woql.Query exposing
    ( Object
    , Predicate
    , Query(..)
    , Subject
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
    | AddTriple Subject Predicate Object
    | IdGen Base Key String


type alias Subject =
    Value


type alias Predicate =
    Value


type alias Object =
    Value


type alias Base =
    Value


type alias Key =
    List Value


type Value
    = Var String
    | Node String
    | Literal String


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

            Triple subject predicate object ->
                [ ( "@type", Encode.string "woql:Triple" )
                , ( "woql:subject", encodeValue subject )
                , ( "woql:predicate", encodeValue predicate )
                , ( "woql:object", encodeValue object )
                ]

            Optional subquery ->
                [ ( "@type", Encode.string "woql:Optional" )
                , ( "woql:query", encodeSubQuery Nothing subquery )
                ]

            AddTriple subject predicate object ->
                [ ( "@type", Encode.string "woql:AddTriple" )
                , ( "woql:subject", encodeValue subject )
                , ( "woql:predicate", encodeValue predicate )
                , ( "woql:object", encodeValue object )
                ]

            IdGen base key uri ->
                [ ( "@type", Encode.string "woql:IDGenerator" )
                , ( "woql:base", encodeValue base )
                , ( "woql:key_list", encodeKeyList key )
                , ( "woql:uri", Encode.string uri )
                ]
        )
            ++ (case context of
                    Nothing ->
                        []

                    Just c ->
                        [ ( "@context", Prefix.encodeContext c ) ]
               )


encodeKeyList : List Value -> Encode.Value
encodeKeyList key =
    Encode.list
        (encodeIndexedListItem "woql:ValueListElement" "woql:value_list_element" encodeValue)
        (List.indexedMap Tuple.pair key)


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

        Literal s ->
            Xsd.encodeString s
