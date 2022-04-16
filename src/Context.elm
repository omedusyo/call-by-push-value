module Context exposing (..)

import Calculus exposing (ValueName, ValueType)
import Dict exposing (Dict)


type alias Context =
    Dict ValueName ValueType


empty : Context
empty =
    Dict.empty


insert : ValueName -> ValueType -> Context -> Context
insert valueName type_ ctx =
    ctx
        |> Dict.insert valueName type_


get : ValueName -> Context -> ValueType
get valueName ctx =
    case ctx |> Dict.get valueName of
        Just type_ ->
            type_

        Nothing ->
            Debug.todo (String.concat [ "Type Check Error: unknown value name `", valueName, "`" ])
