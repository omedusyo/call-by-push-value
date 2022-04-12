module Evaluation exposing (..)

import Calculus exposing (Computation, ComputationType, Value, ValueName, ValueNameIntro, ValueType)
import Dict exposing (Dict)


type Either a b
    = Left a
    | Right b



-- ===Environments===


type alias TypedValue =
    { value : Value
    , type_ : ValueType
    }


type alias TypedComputation =
    { computation : Computation
    , type_ : ComputationType
    }


typedValue : Value -> ValueType -> TypedValue
typedValue value type_ =
    { value = value, type_ = type_ }


typedComputation : Computation -> ComputationType -> TypedComputation
typedComputation computation type_ =
    { computation = computation, type_ = type_ }


type alias Env =
    Dict ValueName (List TypedValue)


emptyEnv : Env
emptyEnv =
    Dict.empty


insertEnv : ValueNameIntro -> Value -> Env -> Env
insertEnv valueIntro val env =
    env
        |> Dict.update valueIntro.name
            (\maybeValues ->
                case maybeValues of
                    Just values ->
                        Just ({ value = val, type_ = valueIntro.type_ } :: values)

                    Nothing ->
                        Just [ { value = val, type_ = valueIntro.type_ } ]
            )


insertsEnv : List ( ValueNameIntro, Value ) -> Env -> Env
insertsEnv bindings env =
    List.foldl (\( varName, val ) envState -> insertEnv varName val envState)
        env
        bindings


getEnv : ValueName -> Env -> TypedValue
getEnv valName env =
    case env |> Dict.get valName |> Maybe.andThen List.head of
        Just tvalue ->
            tvalue

        Nothing ->
            Debug.todo (String.concat [ "Lookup Error: unknown value name `", valName, "`" ])


extractTensor : TypedValue -> ( TypedValue, TypedValue )
extractTensor tvalue =
    case tvalue.type_ of
        Calculus.TensorProduct type0 type1 ->
            case tvalue.value of
                Calculus.TensorProductPair value0 value1 ->
                    ( typedValue value0 type0, typedValue value1 type1 )

                _ ->
                    Debug.todo "Value Error: expected a tensor product of two values"

        _ ->
            Debug.todo "Type Error: you are trying to access a binding of `TensorProduct` type"


extractSum : TypedValue -> Either TypedValue TypedValue
extractSum tvalue =
    case tvalue.type_ of
        Calculus.ValueSum leftType rightType ->
            case tvalue.value of
                Calculus.Left leftValue _ ->
                    Left (typedValue leftValue leftType)

                Calculus.Right _ rightValue ->
                    Right (typedValue rightValue rightType)

                _ ->
                    Debug.todo "Value Error: expected `left val` or `right val`"

        _ ->
            Debug.todo "Type Error: you are trying to access a binding of `Sum` type"


extractBool : TypedValue -> Either TypedValue TypedValue
extractBool tvalue =
    case tvalue.value of
        Calculus.TrueConstant ->
            Left tvalue

        Calculus.FalseConstant ->
            Right tvalue

        _ ->
            Debug.todo "Value Error: expected a boolean value"


extractFreeze : TypedValue -> TypedComputation
extractFreeze tvalue =
    case tvalue.type_ of
        Calculus.Frozen computationType ->
            case tvalue.value of
                Calculus.Freeze computation ->
                    typedComputation computation computationType

                _ ->
                    Debug.todo "Computation Error: expected a frozen computation"

        _ ->
            Debug.todo "Type Error: you are trying to access a binding of `Frozen` type"



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
