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


insertEnv : ValueName -> TypedValue -> Env -> Env
insertEnv valueName tvalue env =
    env
        |> Dict.update valueName
            (\maybeValues ->
                case maybeValues of
                    Just values ->
                        Just (tvalue :: values)

                    Nothing ->
                        Just [ tvalue ]
            )


insertsEnv : List ( ValueName, TypedValue ) -> Env -> Env
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


extractTensorFromValue : Value -> Env -> ( TypedValue, TypedValue )
extractTensorFromValue value env =
    case value of
        Calculus.ValueNameUse name ->
            extractTensor (env |> getEnv name)

        Calculus.TensorProductPair value0 value1 ->
            ( typedValue value0 (env |> typeCheckValue value0)
            , typedValue value1 (env |> typeCheckValue value1)
            )

        _ ->
            Debug.todo "Extraction Error: you are trying to extract from a value of type `TensorProduct`"


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


extractSumFromValue : Value -> Env -> Either TypedValue TypedValue
extractSumFromValue value env =
    case value of
        Calculus.ValueNameUse name ->
            extractSum (env |> getEnv name)

        Calculus.Left valueLeft _ ->
            Left (typedValue valueLeft (env |> typeCheckValue valueLeft))

        Calculus.Right _ valueRight ->
            Right (typedValue valueRight (env |> typeCheckValue valueRight))

        _ ->
            Debug.todo "Extraction Error: you are trying to extract from a value of type `Sum`"


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


extractBoolFromValue : Value -> Env -> Either () ()
extractBoolFromValue value env =
    case value of
        Calculus.ValueNameUse name ->
            extractBool (env |> getEnv name)

        Calculus.TrueConstant ->
            Left ()

        Calculus.FalseConstant ->
            Right ()

        _ ->
            Debug.todo "Extraction Error: you are trying to extract from a value of type `Sum`"


extractBool : TypedValue -> Either () ()
extractBool tvalue =
    case tvalue.value of
        Calculus.TrueConstant ->
            Left ()

        Calculus.FalseConstant ->
            Right ()

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



-- ===Type Checking===


typeCheckValue : Value -> Env -> ValueType
typeCheckValue value env =
    case value of
        Calculus.ValueNameUse valueName ->
            (env |> getEnv valueName).type_

        Calculus.TensorProductPair value0 value1 ->
            Calculus.TensorProduct (env |> typeCheckValue value0) (env |> typeCheckValue value1)

        Calculus.Left valueLeft typeRight ->
            Calculus.ValueSum (env |> typeCheckValue valueLeft) typeRight

        Calculus.Right typeLeft valueRight ->
            Calculus.ValueSum typeLeft (env |> typeCheckValue valueRight)

        Calculus.Freeze computation ->
            Calculus.Frozen (env |> typeCheckComputation computation)

        Calculus.TrueConstant ->
            Calculus.BoolType

        Calculus.FalseConstant ->
            Calculus.BoolType


typeCheckComputation : Computation -> Env -> ComputationType
typeCheckComputation computation env =
    Debug.todo ""



-- ===Small Step Evaluator===


step : Computation -> Env -> Stack -> State
step currentComputation env stack =
    case currentComputation of
        Calculus.MatchTensorProduct value body ->
            let
                ( typedValue0, typedValue1 ) =
                    env |> extractTensorFromValue value
            in
            step
                body.computation
                (env
                    |> insertEnv body.var0 typedValue0
                    |> insertEnv body.var1 typedValue1
                )
                stack

        Calculus.MatchSum value bodyLeft bodyRight ->
            case env |> extractSumFromValue value of
                Left typedValueLeft ->
                    step
                        bodyLeft.computation
                        (env
                            |> insertEnv bodyLeft.var typedValueLeft
                        )
                        stack

                Right typedValueRight ->
                    step
                        bodyRight.computation
                        (env
                            |> insertEnv bodyRight.var typedValueRight
                        )
                        stack

        Calculus.MatchBool value bodyLeft bodyRight ->
            case env |> extractBoolFromValue value of
                Left () ->
                    step
                        bodyLeft.computation
                        env
                        stack

                Right () ->
                    step
                        bodyRight.computation
                        env
                        stack

        _ ->
            Debug.todo ""
