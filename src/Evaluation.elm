module Evaluation exposing (..)

import Dict exposing (Dict)
import Lambda exposing (Computation, ComputationType, Value, ValueName, ValueNameIntro, ValueType)



-- ===Environments===


type alias Env =
    Dict ValueName (List ( ValueType, Value ))


emptyEnv : Env
emptyEnv =
    Dict.empty


insertEnv : ValueNameIntro -> Value -> Env -> Env
insertEnv ( varName, type0 ) val env =
    env
        |> Dict.update varName
            (\maybeValues ->
                case maybeValues of
                    Just values ->
                        Just (( type0, val ) :: values)

                    Nothing ->
                        Just [ ( type0, val ) ]
            )


insertsEnv : List ( ValueNameIntro, Value ) -> Env -> Env
insertsEnv bindings env =
    List.foldl (\( varName, val ) envState -> insertEnv varName val envState)
        env
        bindings


getEnv : ValueName -> Env -> Maybe ( ValueType, Value )
getEnv varName env =
    env |> Dict.get varName |> Maybe.andThen List.head



-- ===Stacks===


type Stack
    = Nil
    | RememberComputation ComputationWithHole Stack


type ComputationWithHole
    = MatchTensorProduct { var0 : ValueNameIntro, var1 : ValueNameIntro, body : Computation }
    | MatchSum { var : ValueNameIntro, body : Computation } { var : ValueNameIntro, body : Computation }


type alias State =
    { env : Env
    , stack : Stack
    , currentComputation : Computation
    }
